'plot cac40
'cac40.correl
'genr LCAC40=log(Cac40)
'plot lcac40
'lcac40.correl

'genr DLCAC40=LCAC40-LCAC40(-1)
'plot dlcac40
'dlcac40.correl(70)

genr DDLCAC40=DLCAC40-DLCAC40(-1)
plot ddlcac40
ddlcac40.correl(70)
