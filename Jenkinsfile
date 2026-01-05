pipeline {
  agent none

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'
    REBAR_CACHE_DIR   = "${WORKSPACE}/.rebar-cache"
  }

  /***********************
   * GLOBAL CLEANUP
   ***********************/
  stages {
    stage('Clean workspace') {
      agent any
      steps {
        sh '''
          set -eux
          rm -rf _build .rebar3 .rebar-cache || true
          rm -rf ~/.cache/rebar3 || true
        '''
      }
    }

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

          stage('Clean') {
            steps {
              sh 'rm -rf _build .rebar3 .rebar-cache || true'
            }
          }

          stage('Run small tests') {
            steps {
              sh 'tools/test.sh -p small_tests -s true -e true'
            }
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

          stage('Clean') {
            steps {
              sh 'rm -rf _build .rebar3 .rebar-cache || true'
            }
          }

          stage('Run big tests') {
            steps {
              sh '''
                SPEC="default.spec"
                if [ "$PRESET" = "elasticsearch_and_cassandra_mnesia" ]; then
                  SPEC="mam.spec"
                fi
                tools/test.sh -p $PRESET -t $SPEC
              '''
            }
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

          stage('Clean') {
            steps {
              sh 'rm -rf _build .rebar3 .rebar-cache || true'
            }
          }

          stage('Run dynamic domain tests') {
            steps {
              sh 'tools/test.sh -p $PRESET -t dynamic_domains.spec'
            }
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
        sh 'rm -rf _build .rebar3 .rebar-cache || true'
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
        sh 'rm -rf _build .rebar3 .rebar-cache || true'
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
        sh 'rm -rf _build .rebar3 .rebar-cache || true'
        sh 'tools/test.sh -p edoc_only'
      }
    }

    /***********************
     * PKG (Ubuntu only)
     ***********************/
    stage('pkg') {
      agent { label 'linux' }   // Ubuntu Jenkins node (NOT Docker)
      environment {
        pkg_OTP_VERSION = '28.0.2'
        pkg_PLATFORM    = 'ubuntu-jammy'
        GPG_PUBLIC_KEY  = credentials('gpg-public-key')
        GPG_PRIVATE_KEY = credentials('gpg-private-key')
        GPG_PASS        = credentials('gpg-pass')
      }
      steps {
        checkout scm

        sh '''
          sudo apt-get update
          sudo apt-get install -y \
            dpkg rpm fakeroot gnupg \
            build-essential curl
        '''

        sh 'rm -rf _build .rebar3 .rebar-cache || true'
        sh 'tools/test.sh -p pkg'
      }
    }
  }
}

