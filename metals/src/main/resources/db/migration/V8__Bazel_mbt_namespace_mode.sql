-- The selected Bazel MBT namespace grouping for each Bazel project root.
create table bazel_mbt_namespace_mode(
  project_root varchar primary key,
  namespace_mode varchar not null,
  when_recorded timestamp
);
