pipeline {
    agent any

    environment {
        CERTS_CACHE_KEY_FILE = "certs_cache_key"
        CERTS_CACHE_DIR = "${WORKSPACE}/.certs-cache"
    }

    triggers {
        pollSCM('')
    }

    stages {

        stage('Clean Workspace') {
            steps {
                cleanWs()
            }
        }

        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Prepare Cert Cache Key') {
            steps {
                sh '''
                    mkdir -p ${CERTS_CACHE_DIR}
                    tools/make-certs-cache-key.sh > ${CERTS_CACHE_KEY_FILE}
                    echo "Cert cache key:"
                    cat ${CERTS_CACHE_KEY_FILE}
                '''
            }
        }

        stage('Restore Certificates Cache') {
            steps {
                sh '''
                    if [ -d "${CERTS_CACHE_DIR}/tools" ]; then
                        echo "Restoring cached certificates"
                        cp -r ${CERTS_CACHE_DIR}/tools ${WORKSPACE}/
                    else
                        echo "No cert cache found"
                    fi
                '''
            }
        }

        stage('Build Certificates If Missing') {
            steps {
                sh '''
                    if [ ! -f tools/ssl/mongooseim/key.pem ]; then
                        echo "Certificates not found, building..."
                        make certs
                    else
                        echo "Certificates already exist, skipping build"
                    fi
                '''
            }
        }

        stage('Print Cert Hashes') {
            steps {
                sh '''
                    find tools/ssl -type f -exec md5sum {} \\; | sort
                '''
            }
        }

        stage('Validate Certificates') {
            steps {
                sh 'test -f tools/ssl/mongooseim/key.pem'
            }
        }

        stage('Save Certificates Cache') {
            steps {
                sh '''
                    rm -rf ${CERTS_CACHE_DIR}/tools
                    mkdir -p ${CERTS_CACHE_DIR}
                    cp -r tools ${CERTS_CACHE_DIR}/
                '''
            }
        }

        stage('Generate Config') {
            steps {
                sh '''
                    tools/circle-generate-config.sh generated_config.yml
                    ls -lh generated_config.yml
                '''
            }
        }

        stage('Make Test') {
            steps {
                sh '''
                    echo "Running tests"
                    make test
                '''
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: 'generated_config.yml', fingerprint: true
        }
        success {
            echo "✅ Pipeline completed successfully"
        }
        failure {
            echo "❌ Pipeline failed"
        }
    }
}
