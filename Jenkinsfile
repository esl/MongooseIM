pipeline {
  agent none

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'

    // Backblaze B2 / S3-compatible
    AWS_DEFAULT_REGION     = 'eu-central-003'
    AWS_ACCESS_KEY_ID     = credentials('aws-access-key-id')
    AWS_SECRET_ACCESS_KEY = credentials('aws-secret-access-key')
  }

  stages {

    /***********************
     * SMALL TESTS
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
            steps { checkout scm }
          }

          stage('Create certificates') {
            steps { sh 'make certs' }
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
                   'ldap_mnesia',
                   'elasticsearch_and_cassandra_mnesia'
          }
          axis {
            name 'OTP'
            values '28.0.2',
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
            steps { checkout scm }
          }

          stage('Run big tests') {
            steps {
              sh 'tools/test.sh -p $PRESET'
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
            values 'pgsql_mnesia', 'mysql_redis'
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
            steps { checkout scm }
          }

          stage('Run dynamic domains tests') {
            steps {
              sh 'tools/test.sh -p $PRESET'
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

    /***********************
     * DIALYZER
     ***********************/
    stage('dialyzer') {
      agent {
        docker {
          image 'erlang:27.3.4.2'
          args '-u root'
        }
      }
      steps {
        checkout scm
        sh 'tools/test.sh -p dialyzer_only'
      }
    }

    /***********************
     * XREF
     ***********************/
    stage('xref') {
      agent {
        docker {
          image 'erlang:27.3.4.2'
          args '-u root'
        }
      }
      steps {
        checkout scm
        sh 'tools/test.sh -p xref_only'
      }
    }

    /***********************
     * EDOC
     ***********************/
    stage('edoc') {
      agent {
        docker {
          image 'erlang:27.3.4.2'
          args '-u root'
        }
      }
      steps {
        checkout scm
        sh 'tools/test.sh -p edoc_only'
      }
    }

    /***********************
     * PKG (HOST UBUNTU ONLY)
     ***********************/
    stage('pkg') {
      agent any   // MUST run on Ubuntu host, NOT Docker
      environment {
        pkg_OTP_VERSION = '28.0.2'
        pkg_PLATFORM    = 'ubuntu-jammy'
        GPG_PUBLIC_KEY  = credentials('gpg-public-key')
        GPG_PRIVATE_KEY = credentials('gpg-private-key')
        GPG_PASS        = credentials('gpg-pass')
      }
      steps {
        checkout scm
        sh 'tools/test.sh -p pkg'
      }
    }
  }
}
