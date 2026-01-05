pipeline {
  agent none

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'
    PRESET             = 'small_tests'
  }

  stages {

    stage('Small Tests Matrix') {
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
            args '--user root'
            reuseNode true
          }
        }

        stages {

          stage('Checkout') {
            steps {
              checkout scm
            }
          }

          stage('Install system dependencies') {
            steps {
              sh '''
                apt-get update
                apt-get install -y \
                  build-essential \
                  gcc \
                  g++ \
                  make \
                  unixodbc-dev \
                  libssl-dev \
                  git \
                  curl
              '''
            }
          }

          stage('Prepare rebar3 cache') {
            steps {
              sh 'mkdir -p ~/.cache/rebar3'
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
            when {
              expression { fileExists('rebar3') }
            }
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
  }
}
