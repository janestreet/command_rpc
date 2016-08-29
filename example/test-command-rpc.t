
 
  $ $TEST_DIR/command_rpc_example.exe caller -version v1 10 11
  result: 21

  $ $TEST_DIR/command_rpc_example.exe caller -version v2 10 11
  result: 21

We have to print stdout and stderr of this command separately because
it can't give any ordering guarantee
  $ STDOUT=$($TEST_DIR/command_rpc_example.exe caller -version v3 10 11)
  hello world via Core.Std
  hello world via Async
  hello world via Async stderr
  hello world via fork&exec

  $ echo "$STDOUT"
  result: 21
