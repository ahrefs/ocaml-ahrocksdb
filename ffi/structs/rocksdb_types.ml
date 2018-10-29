type perf_context =
  User_key_comparison_count
  | Block_cache_hit_count
  | Block_read_count
  | Block_read_byte
  | Block_read_time
  | Block_checksum_time
  | Block_decompress_time
  | Get_read_bytes
  | Multiget_read_bytes
  | Iter_read_bytes
  | Internal_key_skipped_count
  | Internal_delete_skipped_count
  | Internal_recent_skipped_count
  | Internal_merge_count
  | Get_snapshot_time
  | Get_from_memtable_time
  | Get_from_memtable_count
  | Get_post_process_time
  | Get_from_output_files_time
  | Seek_on_memtable_time
  | Seek_on_memtable_count
  | Next_on_memtable_count
  | Prev_on_memtable_count
  | Seek_child_seek_time
  | Seek_child_seek_count
  | Seek_min_heap_time
  | Seek_max_heap_time
  | Seek_internal_seek_time
  | Find_next_user_entry_time
  | Write_wal_time
  | Write_memtable_time
  | Write_delay_time
  | Write_pre_and_post_process_time
  | Db_mutex_lock_nanos
  | Db_condition_wait_nanos
  | Merge_operator_time_nanos
  | Read_index_block_nanos
  | Read_filter_block_nanos
  | New_table_block_iter_nanos
  | New_table_iterator_nanos
  | Block_seek_nanos
  | Find_table_nanos
  | Bloom_memtable_hit_count
  | Bloom_memtable_miss_count
  | Bloom_sst_hit_count
  | Bloom_sst_miss_count
  | Key_lock_wait_time
  | Key_lock_wait_count
  | Env_new_sequential_file_nanos
  | Env_new_random_access_file_nanos
  | Env_new_writable_file_nanos
  | Env_reuse_writable_file_nanos
  | Env_new_random_rw_file_nanos
  | Env_new_directory_nanos
  | Env_file_exists_nanos
  | Env_get_children_nanos
  | Env_get_children_file_attributes_nanos
  | Env_delete_file_nanos
  | Env_create_dir_nanos
  | Env_create_dir_if_missing_nanos
  | Env_delete_dir_nanos
  | Env_get_file_size_nanos
  | Env_get_file_modification_time_nanos
  | Env_rename_file_nanos
  | Env_link_file_nanos
  | Env_lock_file_nanos
  | Env_unlock_file_nanos
  | Env_new_logger_nanos
  | Total_metric_count


module Struct_stubs(S : Ctypes.TYPE) = struct

  open S

  let user_key_comparison_count =              constant  "rocksdb_user_key_comparison_count" int64_t
  let block_cache_hit_count =                  constant  "rocksdb_block_cache_hit_count" int64_t
  let block_read_count =                       constant  "rocksdb_block_read_count" int64_t
  let block_read_byte =                        constant  "rocksdb_block_read_byte" int64_t
  let block_read_time =                        constant  "rocksdb_block_read_time" int64_t
  let block_checksum_time =                    constant  "rocksdb_block_checksum_time" int64_t
  let block_decompress_time =                  constant  "rocksdb_block_decompress_time" int64_t
  let get_read_bytes =                         constant  "rocksdb_get_read_bytes" int64_t
  let multiget_read_bytes =                    constant  "rocksdb_multiget_read_bytes" int64_t
  let iter_read_bytes =                        constant  "rocksdb_iter_read_bytes" int64_t
  let internal_key_skipped_count =             constant  "rocksdb_internal_key_skipped_count" int64_t
  let internal_delete_skipped_count =          constant  "rocksdb_internal_delete_skipped_count" int64_t
  let internal_recent_skipped_count =          constant  "rocksdb_internal_recent_skipped_count" int64_t
  let internal_merge_count =                   constant  "rocksdb_internal_merge_count" int64_t
  let get_snapshot_time =                      constant  "rocksdb_get_snapshot_time" int64_t
  let get_from_memtable_time =                 constant  "rocksdb_get_from_memtable_time" int64_t
  let get_from_memtable_count =                constant  "rocksdb_get_from_memtable_count" int64_t
  let get_post_process_time =                  constant  "rocksdb_get_post_process_time" int64_t
  let get_from_output_files_time =             constant  "rocksdb_get_from_output_files_time" int64_t
  let seek_on_memtable_time =                  constant  "rocksdb_seek_on_memtable_time" int64_t
  let seek_on_memtable_count =                 constant  "rocksdb_seek_on_memtable_count" int64_t
  let next_on_memtable_count =                 constant  "rocksdb_next_on_memtable_count" int64_t
  let prev_on_memtable_count =                 constant  "rocksdb_prev_on_memtable_count" int64_t
  let seek_child_seek_time =                   constant  "rocksdb_seek_child_seek_time" int64_t
  let seek_child_seek_count =                  constant  "rocksdb_seek_child_seek_count" int64_t
  let seek_min_heap_time =                     constant  "rocksdb_seek_min_heap_time" int64_t
  let seek_max_heap_time =                     constant  "rocksdb_seek_max_heap_time" int64_t
  let seek_internal_seek_time =                constant  "rocksdb_seek_internal_seek_time" int64_t
  let find_next_user_entry_time =              constant  "rocksdb_find_next_user_entry_time" int64_t
  let write_wal_time =                         constant  "rocksdb_write_wal_time" int64_t
  let write_memtable_time =                    constant  "rocksdb_write_memtable_time" int64_t
  let write_delay_time =                       constant  "rocksdb_write_delay_time" int64_t
  let write_pre_and_post_process_time =        constant  "rocksdb_write_pre_and_post_process_time" int64_t
  let db_mutex_lock_nanos =                    constant  "rocksdb_db_mutex_lock_nanos" int64_t
  let db_condition_wait_nanos =                constant  "rocksdb_db_condition_wait_nanos" int64_t
  let merge_operator_time_nanos =              constant  "rocksdb_merge_operator_time_nanos" int64_t
  let read_index_block_nanos =                 constant  "rocksdb_read_index_block_nanos" int64_t
  let read_filter_block_nanos =                constant  "rocksdb_read_filter_block_nanos" int64_t
  let new_table_block_iter_nanos =             constant  "rocksdb_new_table_block_iter_nanos" int64_t
  let new_table_iterator_nanos =               constant  "rocksdb_new_table_iterator_nanos" int64_t
  let block_seek_nanos =                       constant  "rocksdb_block_seek_nanos" int64_t
  let find_table_nanos =                       constant  "rocksdb_find_table_nanos" int64_t
  let bloom_memtable_hit_count =               constant  "rocksdb_bloom_memtable_hit_count" int64_t
  let bloom_memtable_miss_count =              constant  "rocksdb_bloom_memtable_miss_count" int64_t
  let bloom_sst_hit_count =                    constant  "rocksdb_bloom_sst_hit_count" int64_t
  let bloom_sst_miss_count =                   constant  "rocksdb_bloom_sst_miss_count" int64_t
  let key_lock_wait_time =                     constant  "rocksdb_key_lock_wait_time" int64_t
  let key_lock_wait_count =                    constant  "rocksdb_key_lock_wait_count" int64_t
  let env_new_sequential_file_nanos =          constant  "rocksdb_env_new_sequential_file_nanos" int64_t
  let env_new_random_access_file_nanos =       constant  "rocksdb_env_new_random_access_file_nanos" int64_t
  let env_new_writable_file_nanos =            constant  "rocksdb_env_new_writable_file_nanos" int64_t
  let env_reuse_writable_file_nanos =          constant  "rocksdb_env_reuse_writable_file_nanos" int64_t
  let env_new_random_rw_file_nanos =           constant  "rocksdb_env_new_random_rw_file_nanos" int64_t
  let env_new_directory_nanos =                constant  "rocksdb_env_new_directory_nanos" int64_t
  let env_file_exists_nanos =                  constant  "rocksdb_env_file_exists_nanos" int64_t
  let env_get_children_nanos =                 constant  "rocksdb_env_get_children_nanos" int64_t
  let env_get_children_file_attributes_nanos = constant  "rocksdb_env_get_children_file_attributes_nanos" int64_t
  let env_delete_file_nanos =                  constant  "rocksdb_env_delete_file_nanos" int64_t
  let env_create_dir_nanos =                   constant  "rocksdb_env_create_dir_nanos" int64_t
  let env_create_dir_if_missing_nanos =        constant  "rocksdb_env_create_dir_if_missing_nanos" int64_t
  let env_delete_dir_nanos =                   constant  "rocksdb_env_delete_dir_nanos" int64_t
  let env_get_file_size_nanos =                constant  "rocksdb_env_get_file_size_nanos" int64_t
  let env_get_file_modification_time_nanos =   constant  "rocksdb_env_get_file_modification_time_nanos" int64_t
  let env_rename_file_nanos =                  constant  "rocksdb_env_rename_file_nanos" int64_t
  let env_link_file_nanos =                    constant  "rocksdb_env_link_file_nanos" int64_t
  let env_lock_file_nanos =                    constant  "rocksdb_env_lock_file_nanos" int64_t
  let env_unlock_file_nanos =                  constant  "rocksdb_env_unlock_file_nanos" int64_t
  let env_new_logger_nanos =                   constant  "rocksdb_env_new_logger_nanos" int64_t
  let total_metric_count =                     constant  "rocksdb_total_metric_count" int64_t

end
