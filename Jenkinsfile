pipeline {
  agent none

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'

    AWS_DEFAULT_REGION      = credentials('AWS_DEFAULT_REGION')
    AWS_ACCESS_KEY_ID      = credentials('AWS_ACCESS_KEY_ID')
    AWS_SECRET_ACCESS_KEY  = credentials('AWS_SECRET_ACCESS_KEY')

    GA4_API_SECRET         = credentials('GA4_API_SECRET')
    GA4_MEASUREMENT_API    = credentials('GA4_MEASUREMENT_API')
  }

  options {
    timestamps()
    ansiColor('xterm')
  }

  stages {

    /* ================= SMALL TESTS ================= */

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

        environment {
          PRESET = 'small_tests'
        }

        stages {
          stage('Checkout') {
            steps {
              checkout scm
            }
          }

          stage('Cache rebar3') {
            steps {
              sh 'mkdir -p ~/.cache/rebar3'
            }
          }

          stage('Create certs') {
            steps {
              sh 'make certs'
            }
          }

          stage('Run tests') {
            steps {
              sh 'tools/test.sh -p small_tests -s true -e true'
            }
          }

          stage('Coverage') {
            steps {
              sh './rebar3 codecov analyze --lcov --json false'
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

    /* ================= BIG TESTS ================= */

    stage('big_tests') {
      parallel {

        stage('internal_mnesia') {
          steps { runBigTests('internal_mnesia', '28.0.2', 'default.spec') }
        }

        stage('pgsql_mnesia') {
          steps { runBigTests('pgsql_mnesia', '27.3.4.2', 'default.spec') }
        }

        stage('ldap_mnesia') {
          steps { runBigTests('ldap_mnesia', '27.3.4.2', 'default.spec') }
        }

        stage('elasticsearch_and_cassandra_mnesia') {
          steps { runBigTests('elasticsearch_and_cassandra_mnesia', '28.0.2', 'mam.spec') }
        }
      }
    }

    /* ================= DIALYZER / XREF / EDOC ================= */

    stage('static_checks') {
      parallel {

        stage('dialyzer') {
          agent { docker { image 'erlang:27.3.4.2' } }
          steps {
            checkout scm
            sh 'tools/test.sh -p dialyzer_only'
          }
        }

        stage('xref') {
          agent { docker { image 'erlang:27.3.4.2' } }
          steps {
            checkout scm
            sh 'tools/test.sh -p xref_only'
          }
        }

        stage('edoc') {
          agent { docker { image 'erlang:27.3.4.2' } }
          steps {
            checkout scm
            sh 'tools/test.sh -p edoc_only'
          }
        }
      }
    }

    /* ================= PACKAGE ================= */

    stage('pkg') {
      agent {
        docker {
          image 'erlang:28.0.2'
          args '--privileged'
        }
      }

      environment {
        pkg_OTP_VERSION = '28.0.2'
        pkg_PLATFORM    = 'ubuntu-jammy'
      }

      steps {
        checkout scm
        sh 'tools/test.sh -p pkg'

        sh '''
          IMAGE_TAG=$(docker images mongooseim-ubuntu-jammy --format "{{.Tag}}" | head -n1)
          docker save mongooseim-ubuntu-jammy:$IMAGE_TAG | gzip > mongooseim-ubuntu-jammy.tar.gz
        '''

        archiveArtifacts artifacts: 'mongooseim-ubuntu-jammy.tar.gz'
      }
    }
  }
}

/* ================= HELPER FUNCTION ================= */

def runBigTests(String preset, String otp, String spec) {
  docker.image("erlang:${otp}").inside('-u root') {
    checkout scm
    sh """
      tools/test.sh \
        -p ${preset} \
        -s true \
        -e true \
        -t ${spec}
    """
  }
}
