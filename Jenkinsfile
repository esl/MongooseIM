pipeline {
  agent none

  options {
    timestamps()
    disableConcurrentBuilds()
  }

  environment {
    SKIP_CERT_BUILD = '1'
    SKIP_AUTO_COMPILE = 'true'
  }

  stages {

    stage('Checkout') {
      agent any
      steps {
        checkout scm
        sh 'git fetch --tags'
        sh 'git describe --tags --exact-match || true'
      }
    }

    stage('Build & Test (OTP 28)') {
      agent {
        docker {
          image 'erlangsolutions/erlang:cimg-28.0.2'
          args '--network host -v /var/run/docker.sock:/var/run/docker.sock'
        }
      }

      options {
        skipDefaultCheckout()
      }

      environment {
        REBAR_CACHE_DIR = "${WORKSPACE}/.rebar3"
        HEX_HOME        = "${WORKSPACE}/.hex"
        HOME            = "${WORKSPACE}"
      }

      stages {

        stage('Prepare Certs') {
          steps {
            sh '''
              tools/make-certs-cache-key.sh || true
              test -f tools/ssl/mongooseim/key.pem || make certs
            '''
          }
        }

        stage('Build Dependencies') {
          steps {
            sh '''
              tools/configure
              tools/build-deps.sh
              tools/build-test-deps.sh
            '''
          }
        }

        stage('Compile') {
          steps {
            sh './rebar3 compile'
          }
        }

        stage('Build Release') {
          steps {
            sh '''
              make rel
              make xeplist
            '''
          }
        }

        stage('Small Tests') {
          steps {
            sh 'tools/test.sh -p small_tests -s true -e true'
          }
        }

        stage('Big Tests (pgsql_mnesia)') {
          environment {
            PRESET = 'pgsql_mnesia'
            DB = 'postgres redis'
            TLS_DIST = 'true'
          }
          steps {
            sh 'tools/test.sh -p pgsql_mnesia -s false'
          }
        }
      }
    }

    stage('Build & Push Docker Image') {
      when {
        buildingTag()
      }
      agent {
        docker {
          image 'docker:26'
          args '-v /var/run/docker.sock:/var/run/docker.sock'
        }
      }
      steps {
        withCredentials([
          usernamePassword(
            credentialsId: 'dockerhub-creds',
            usernameVariable: 'DOCKER_USER',
            passwordVariable: 'DOCKER_PASS'
          )
        ]) {
          sh '''
            echo "$DOCKER_PASS" | docker login -u "$DOCKER_USER" --password-stdin
            make docker
            make docker-push
          '''
        }
      }
    }
  }

  post {
    always {
      node {
        sh 'ls -lh _build || true'
      }
    }
    failure {
      node {
        sh 'tail -100 _build/*/rel/mongooseim/log/*.log || true'
      }
    }
  }
}
