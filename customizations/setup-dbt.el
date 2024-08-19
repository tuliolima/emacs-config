(use-package dbt-mode
  :load-path "~/dev/dbt-mode"
  :hook (sql-mode . dbt-mode)
  :custom (dbt-mode-python-virtual-env "~/dev/bp-dbt-dw/.venv")
  :init
  (setenv "DBT_TARGET" "dev")
  (setenv "DBT_USER" "dbt_tabner")
  (setenv "DBT_KEYFILE_PATH" "credentials/dbt-staging.json")
  (setenv "DBT_UPSTREAM_PROD_ENABLED" "True"))
