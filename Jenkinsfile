pipeline {
  agent none

  options {
    timestamps()
    timeout(time: 4, unit: 'HOURS')
  }

  environment {
    KEEP_COVER_RUNNING = '1'
    SKIP_AUTO_COMPILE  = 'true'

    AWS_DEFAULT_REGION     = 'eu-central-003'
    AWS_ACCESS_KEY_ID     = credentials('aws-access-key-id')
    AWS_SECRET_ACCESS_KEY = credentials('aws-secret-access-key')

    GPG_PUBLIC_KEY  = credentials('gpg-public-key')
    GPG_PRIVATE_KEY = credentials('gpg-private-key')
    GPG_PASS        = credentials('gpg-pass')
  }

  stages {

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
            args '-u root --privileged'
          }
        }

        stages {
          stage('Install deps') {
            steps {
              sh '''
                apt-get update
                apt-get install -y \
                  build-essential \
                  unixodbc-dev \
                  libssl-dev \
                  libncurses-dev \
                  git curl ca-certificates
              '''
            }
          }

          stage('Checkout') {
            steps { checkout scm }
          }

          stage('Certs') {
            steps { sh 'make certs' }
          }

          stage('Run small tests') {
            steps {
              sh 'tools/test.sh -p small_tests -s true -e true'
            }
          }
        }
      }
    }

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
          stage('Deps') {
            steps {
              sh '''
                apt-get update
                apt-get install -y \
                  build-essential \
                  unixodbc-dev \
                  libssl-dev \
                  libncurses-dev
              '''
            }
          }

          stage('Checkout') {
            steps { checkout scm }
          }

          stage('Run big tests') {
            steps {
              sh '''
                SPEC="default.spec"
                if [ "$PRESET" = "elasticsearch_and_cassandra_mnesia" ]; then
                  SPEC="mam.spec"
                fi
                tools/test.sh -p "$PRESET" -t "$SPEC"
              '''
            }
          }
        }
      }
    }

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
          stage('Deps') {
            steps {
              sh '''
                apt-get update
                apt-get install -y \
                  build-essential \
                  unixodbc-dev
              '''
            }
          }

          stage('Checkout') {
            steps { checkout scm }
          }

          stage('Run tests') {
            steps {
              sh 'tools/test.sh -p $PRESET -t dynamic_domains.spec'
            }
          }
        }
      }
    }

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

    stage('pkg') {
      agent any
      environment {
        pkg_OTP_VERSION = '28.0.2'
        pkg_PLATFORM    = 'ubuntu-jammy'
      }
      steps {
        checkout scm
        sh '''
          which dpkg
          which fakeroot
          which gpg
          tools/test.sh -p pkg
        '''
      }
    }
  }

  post {
    always {
      archiveArtifacts artifacts: '_build/**/log/**,ct.log', allowEmptyArchive: true
    }
    success {
      echo 'CI SUCCESS'
    }
    failure {
      echo 'CI FAILED'
    }
  }
}
