## Release v0.16.0

- In `Command_rpc.Connection.create`, change how stderr handling is specified:
  * `Propagate_stderr` corresponds to old setting `~propagate_stderr:true`
  * `Ignore_stderr`corresponds to `~propagate_stderr:false`
  * `Custom _` lets the user specify custom handling of stderr

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## 114.06.00

- Initial release, forked out of Async_extended.
