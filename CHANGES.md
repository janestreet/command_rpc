## Release v0.17.0

- More useful default connection description, instead of "<created-directly>"

- Release v0.16 silently introduced a new option `new_fds_for_rpc` that supports
  communication over fds other than stdin&stdout.
  A few fixes and tweaks around that:

    * More flexible configuration of `stdout_handling`
    * Fixed an fd leak
    * Propagate `buffer_age_ilmit` correctly

- Make the default rpc timeouts apply consistently (both in `Connection.create` and
  in `Connection.with_close`. 
  
- Optimization: use `Writer.splice` to transfer stderr/stdout

## Release v0.16.0

- In `Command_rpc.Connection.create`, change how stderr handling is specified:
  * `Propagate_stderr` corresponds to old setting `~propagate_stderr:true`
  * `Ignore_stderr`corresponds to `~propagate_stderr:false`
  * `Custom _` lets the user specify custom handling of stderr

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## 114.06.00

- Initial release, forked out of Async_extended.
