'genr Lcac40=log(cac40)
'plot Lcac40
'Lcac40.correl(60)
'genr DLcac40=Lcac40-Lcac40(-1)
'plot DLcac40
'DLcac40.correl(80)
genr DDLcac40=DLcac40-DLcac40(-1)
plot DDLcac40
DDLcac40.correl(80)

