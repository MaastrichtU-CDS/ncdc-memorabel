Package: {{PKG_NAME}}
Type: Package
Version: 0.1.0
Title: {{PKG_NAME}}
Description: This package can be used with the vantage6 federated
    learning infrastructure (see https://github.com/IKNL/vantage6).
Encoding: UTF-8
LazyData: true
Depends:
    dplyr
Imports:
    haven,
    tidyverse,
    vtg,
    modelsummary,
    broom,
    DBI,
    RPostgres
Remotes:
    pedro-cmat/vtg@f10b361409c4a2bb2ee0e929a55933bee941e5a0
RoxygenNote: 7.0.0
