pipeline {
  agent any

  environment {
    IMAGE_NAME = "yourdockerhubuser/mongooseim"
    DOCKER_CREDS = "dockerhub-creds"
  }

  options {
    timestamps()
    disableConcurrentBuilds()
  }

  stages {

    stage('Checkout') {
      steps {
        checkout scm
        script {
          env.GIT_SHA = sh(
            script: "git rev-parse --short HEAD",
            returnStdout: true
          ).trim()
        }
      }
    }

    stage('Build Docker Image') {
      steps {
        sh """
          docker build \
            -t ${IMAGE_NAME}:${GIT_SHA} \
            -t ${IMAGE_NAME}:latest \
            .
        """
      }
    }

    stage('Docker Login') {
      steps {
        withCredentials([usernamePassword(
          credentialsId: "${DOCKER_CREDS}",
          usernameVariable: 'DOCKER_USER',
          passwordVariable: 'DOCKER_PASS'
        )]) {
          sh """
            echo "$DOCKER_PASS" | docker login \
              -u "$DOCKER_USER" --password-stdin
          """
        }
      }
    }

    stage('Push Image') {
      steps {
        sh """
          docker push ${IMAGE_NAME}:${GIT_SHA}
          docker push ${IMAGE_NAME}:latest
        """
      }
    }
  }

  post {
    always {
      sh "docker logout || true"
    }

    success {
      echo "✅ Docker image pushed successfully"
    }

    failure {
      echo "❌ Docker build or push failed"
    }
  }
}
