#!/usr/bin/env bash

set -e
set -x

PROJECT_VERSION=`lein project-version`

if [[ "$PROJECT_VERSION" == *-SNAPSHOT ]]
then
    echo "This job does not support deploying SNAPSHOT artifacts; project version is '$PROJECT_VERSION'"
    exit 1
fi

git fetch --tags

set +e
git show-ref --tags --quiet --verify -- "refs/tags/$PROJECT_VERSION"
TAG_EXISTS=$?
set -e
if [ $TAG_EXISTS -eq 0 ]
then
    echo "A tag called '$PROJECT_VERSION' already exists for this project; aborting."
fi

echo "Project version is valid, tag doesn't yet exist.  Running tests."
lein test
echo "Tests passed!"

git tag $PROJECT_VERSION
git push --tags

echo "Tagged version '$PROJECT_VERSION'"

lein deploy
