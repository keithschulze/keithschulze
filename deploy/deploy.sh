#!/bin/bash

set -euo pipefail

hash aws || { echo "Missing dependency aws" >&2; exit 1; }

[[ ! -z "$1" ]] && DomainName="$1" || { echo "Domain name must be passed as a parameter to script" >&2; exit 1; }

StackName="keithschulze-stack"

aws sts get-caller-identity >/dev/null 2>&1 || { echo "Can't find AWS credentials" >&2; exit 1; }

deploy_cloudformation() {
  local templateFile=$1
  local stackName=$2
  local params="${@:3}"

  aws cloudformation deploy \
      --template-file "$templateFile" \
      --stack-name "$stackName" \
      --no-fail-on-empty-changeset \
      --parameter-overrides $params
}

deploy_cloudformation "deploy/s3-website.yaml" "$StackName" "RootDomainName=$DomainName"

BucketName=$(aws cloudformation describe-stacks --stack-name "$StackName" --query 'Stacks[0].Outputs[?OutputKey==`BucketURL`].OutputValue' --output text)
aws s3 sync _site "s3://$BucketName/"
