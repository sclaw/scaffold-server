#!/bin/bash

grpc_tools_node_protoc --proto_path=../proto --js_out=import_style=commonjs,binary:proto  ../proto/*

