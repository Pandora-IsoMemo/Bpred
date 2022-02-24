pipeline {
    agent any
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'bpred' // github repo name
        CUR_PKG_FOLDER = '.' // defaults to root
        TMP_SUFFIX = """${sh(returnStdout: true, script: 'echo `cat /dev/urandom | tr -dc \'a-z\' | fold -w 6 | head -n 1`')}"""
        GH_TOKEN = credentials("github-isomemo")
    }
    stages {
        stage('Deploy R-package') {
            when { branch 'main' }
            steps {
                sh '''
                curl https://raw.githubusercontent.com/Pandora-IsoMemo/drat/main/deploy.sh > deploy.sh
                # Expects environment variables:
                # CUR_PROJ
                # TMP_SUFFIX
                # GH_TOKEN_PSW -- a GitHub personal access token with write access to the drat repo
                mv Dockerfile Dockerfile.bak
                mv Dockerfile.Testing Dockerfile
                bash deploy.sh
                mv Dockerfile Dockerfile.Testing
                mv Dockerfile.bak Dockerfile
                '''
            }
        }
    }
}
