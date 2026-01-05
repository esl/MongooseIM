pipeline {
  agent none

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'

    // AWS creds from Jenkins Credentials
    AWS_DEFAULT_REGION     = 'u-central-003'
    AWS_ACCESS_KEY_ID     = credentials('aws-access-key-id')
    AWS_SECRET_ACCESS_KEY = credentials('aws-secret-access-key')

    // Optional (only if scripts need them)
    GA4_API_SECRET        = credentials('ga4-api-secret')
    GA4_MEASUREMENT_API   = credentials('ga4-measurement-api')
  }

  stages {

    /***********************
     * SMALL TESTS (KEEP)
     ***********************/
    stage('small_tests') {
      matrix {
        axes {
          axis {
            name 'OTP'
            values '27.3.4.2', '28.0.2'
          }
        }

        agent {
          docker {
            image "erlang:${OTP}"
            args '-u root'
          }
        }

        stages {
          stage('Checkout') {
            steps {
              checkout scm
            }
          }

          stage('Create certificates') {
            steps {
              sh 'make certs'
            }
          }

          stage('Run small tests') {
            steps {
              sh 'tools/test.sh -p small_tests -s true -e true'
            }
          }

          stage('Prepare coverage') {
            steps {
              sh './rebar3 codecov analyze --lcov --json false || true'
            }
          }
        }

        post {
          failure {
            sh 'tools/gh-upload-to-s3.sh _build/test/logs test_logs || true'
          }
        }
      }
    }

    /***********************
     * BIG TESTS
     ***********************/
    stage('big_tests') {
      matrix {
        axes {
          axis {
            name 'PRESET'
            values 'internal_mnesia',
                   'pgsql_mnesia',
                   'mysql_redis',
                   'odbc_mssql_mnesia',
                   'ldap_mnesia',
                   'elasticsearch_and_cassandra_mnesia'
          }
          axis {
            name 'OTP'
            values '28.0.2'
          }
        }

        agent {
          docker {
            image "erlang:${OTP}"
            args '-u root --privileged'
          }
        }

        stages {
          stage('Checkout') {
            steps {
              checkout scm
            }
          }

          stage('Run big tests') {
            steps {
              sh '''
                TEST_SPEC="default.spec"
                if [ "$PRESET" = "elasticsearch_and_cassandra_mnesia" ]; then
                  TEST_SPEC="mam.spec"
                fi

                tools/test.sh -p $PRESET -t $TEST_SPEC
              '''
            }
          }
        }

        post {
          failure {
            sh 'tools/gh-upload-to-s3.sh big_tests/ct_report || true'
          }
        }
      }
    }

    /********************************
     * DYNAMIC DOMAINS BIG TESTS
     ********************************/
    stage('dynamic_domains_big_tests') {
      matrix {
        axes {
          axis {
            name 'PRESET'
            values 'pgsql_mnesia',
                   'mysql_redis',
                   'odbc_mssql_mnesia'
          }
          axis {
            name 'OTP'
            values '28.0.2'
          }
        }

        agent {
          docker {
            image "erlang:${OTP}"
            args '-u root --privileged'
          }
        }

        stages {
          stage('Checkout') {
            steps {
              checkout scm
            }
          }

          stage('Run dynamic domains tests') {
            steps {
              sh 'tools/test.sh -p $PRESET -t dynamic_domains.spec'
            }
          }
        }

        post {
          failure {
            sh 'tools/gh-upload-to-s3.sh big_tests/ct_report || true'
          }
        }
      }
    }
  }
}
