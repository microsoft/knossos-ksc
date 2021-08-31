#!/bin/bash
az acr login -n knossos --expose-token --query accessToken --output tsv
