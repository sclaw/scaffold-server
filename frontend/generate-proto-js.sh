#!/bin/bash

grpc_tools_node_protoc --proto_path=../proto  --js_out=import_style=commonjs,binary:proto \
   ../proto/Time/* \
   ../proto/Protobuf/* \
   ../proto/EdgeNode/User.proto \
   ../proto/EdgeNode/Rbac.proto \
   ../proto/EdgeNode/Error.proto \
   ../proto/EdgeNode/Api/Http/Auth/* \
   ../proto/EdgeNode/Service/* \
   ../proto/EdgeNode/User/*
