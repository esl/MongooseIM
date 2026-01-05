pipeline {
  agent any

  options {
    timestamps()
    disableConcurrentBuilds()
  }

  triggers {
    // Trigger via GitHub webhook or polling
    githubPush()
  }

  environment {
    SKIP_CERT_BUILD   = '1'
    SKIP_AUTO_COMPILE = 'true'
  }

  stages {

    /* ============================================================
       CHECKOUT
       ============================================================ */
    stage('Checkout') {
      steps {
        checkout scm
        sh 'git describe --tags --exact-match || true'
      }
    }

    /* ============================================================
       CIRCLECI DYNAMIC CONFIG (TEMP – FOR PARITY)
       ============================================================ */
    stage('Generate CircleCI Config') {
      steps {
        sh '''
          tools/circle-generate-config.sh generated_config.yml
          ls -lh generated_config.yml
        '''
      }
    }

    /* ============================================================
       CERTS (CACHED)
       ============================================================ */
    stage('Prepare Certs') {
      steps {
        sh '''
          tools/make-certs-cache-key.sh > certs_cache_key || true
          test -f tools/ssl/mongooseim/key.pem || make certs
        '''
      }
    }

    /* ============================================================
       BUILD DEPENDENCIES
       ============================================================ */
    stage('Build Dependencies') {
      steps {
        sh '''
          tools/configure with-all
          tools/build-deps.sh
          tools/build-test-deps.sh
        '''
      }
    }

    /* ============================================================
       COMPILE
       ============================================================ */
    stage('Compile') {
      steps {
        sh './rebar3 compile'
      }
    }

    /* ============================================================
       RELEASE BUILD
       ============================================================ */
    stage('Build Release Artifacts') {
      steps {
        sh '''
          ./tools/build-releases.sh
          make rel
          make xeplist
        '''
      }
    }

    /* ============================================================
       SMALL TESTS
       ============================================================ */
    stage('Small Tests (OTP 28)') {
      agent {
        docker {
          image 'erlangsolutions/erlang:cimg-28.0.2'
          args '--network host'
        }
      }
      steps {
        sh '''
          tools/wait-for-it.sh -p 6379 || true
          tools/test.sh -p small_tests -s true -e true
        '''
      }
    }

    /* ============================================================
       BIG TESTS
       ============================================================ */
    stage('Big Tests (pgsql_mnesia)') {
      agent {
        docker {
          image 'erlangsolutions/erlang:cimg-28.0.2'
          args '--network host'
        }
      }
      environment {
        PRESET   = 'pgsql_mnesia'
        DB       = 'postgres redis'
        TLS_DIST = 'true'
      }
      steps {
        sh '''
          ./tools/circle-wait-for-db.sh
          ./tools/test.sh -p pgsql_mnesia -s false
        '''
      }
    }

    /* ============================================================
       DOCKER BUILD & PUSH (TAG ONLY)
       ============================================================ */
    stage('Build & Push Docker Image') {
      when {
        buildingTag()
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
            docker version
            echo "$DOCKER_PASS" | docker login -u "$DOCKER_USER" --password-stdin

            # CircleCI-equivalent docker build & push
            tools/circle-build-and-push-docker.sh

            docker logout || true
          '''
        }
      }
    }

    /* ============================================================
       DOCKER SMOKE TEST
       ============================================================ */
    stage('Docker Smoke Test') {
      when {
        buildingTag()
      }
      steps {
        sh '''
          source tools/circleci-prepare-mongooseim-docker.sh
          ./smoke_test.sh
        '''
      }
    }
  }

  /* ============================================================
     POST ACTIONS
     ============================================================ */
  post {
    always {
      sh 'tools/prepare-log-dir.sh || true'
    }

    success {
      archiveArtifacts artifacts: '''
        generated_config.yml
        _build/**/rel/mongooseim/**
      ''', fingerprint: true
    }

    failure {
      sh 'tail -100 _build/*/rel/mongooseim/log/*.log || true'
    }
  }
}
