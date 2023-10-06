library(admiral)
data()

adlb_tbl <- copy_to(sc,admiral_adlb,"adlb")

admiral_adsl

adsl_tbl <- copy_to(sc,admiral_adsl,"adsl")


join <- adlb_tbl %>%
        left_join(adsl_tbl%>%select(USUBJID,SAFFL),by= 'USUBJID')

class(join)

