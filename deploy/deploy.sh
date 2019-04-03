#!/bin/bash

set -euo pipefail

hash aws || { echo "Missing dependency aws" >&2; exit 1; }

[[ ! -z "$1" ]] && DomainName="$1" || { echo "Domain name must be passed as a parameter to script" >&2; exit 1; }

StackName="keithschulze-stack"

aws sts get-caller-identity >/dev/null 2>&1 || { echo "Can't find AWS credentials" >&2; exit 1; }

cloudformation_deploy() {
    local STDERR=$(( aws cloudformation deploy "$@" ) 2>&1)
    local EXIT_CODE=$?
    echo ${STDERR} 1>&2
    if [[ "${EXIT_CODE}" -eq "255" && ("${STDERR}" =~ "No changes to deploy" || "${STDERR}" =~ "No updates are to be performed" || "${STDERR}" =~ "The submitted information didn't contain changes") ]]; then
        return 0;
    fi
    return ${EXIT_CODE}
}

cloudformation_deploy --template-file deploy/s3-website.yaml --stack-name $StackName --parameter-override RootDomainName=$DomainName

# BucketURL=$(aws cloudformation describe-stacks --stack-name $StackName --query 'Stacks[0].Outputs[?OutputKey==`BucketURL`].OutputValue' --output text)
#
# aws s3 sync _site s3://$BucketURL/