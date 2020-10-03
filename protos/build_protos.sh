PYTHON_OUT=learning/
HASKELL_OUT=src/
PROTOS=protos

protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
       --python_out="$PYTHON_OUT" \
       --haskell_out="$HASKELL_OUT" \
       "$PROTOS"/Embeddable.proto \
       "$PROTOS"/ChoicePoint.proto \
       "$PROTOS"/Result.proto \
       "$PROTOS"/Label.proto \
       "$PROTOS"/DataPoint.proto \
       "$PROTOS"/Command.proto \
       "$PROTOS"/Response.proto \
