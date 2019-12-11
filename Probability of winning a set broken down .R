pga= 0.5
qga= 1-pga

pgb=0.5
qgb=1-pgb

A1=(pga*qgb)^3

A2=3*(pga)^3*(qga)*(qgb)^3
  +3*(pga)^4*(pgb)*(qgb)^2

A3=12*(pga)^3*(qga)*(pgb)*(qgb)^3
  +6*(pga)^2*(qga)^2*(qgb)^4
  +3*(pga)^4*(pgb)^2*(qgb)^2

A4=24*(pga)^3*(qga)^2*(pgb)*(qgb)^3
  +24*(pga)^4*(qga)*(pgb)^2*(qgb)^2
  +4*(pga)^2*(qga)^3*(qgb)^4
  +4*(pga)^5*(pgb)^3*(qgb)

A5=60*(pga)^3*(qga)^2*(pgb)^2*(qgb)^3
  +40*(pga)^2*(qga)^3*(pgb)*(qga)^4
  +20*(pga)^4*(qga)*(pgb)^3*(qgb)^2
  +5*(pga)*(qga)^4*(qgb)^5
  +(pga)^5*(pgb)^4*(qgb)

A6=100*(pga)^3*(qga)^3*(pgb)^2*(qgb)^4
+100*(pga)^4*(qga)^2*(pgb)^3*(qgb)^3
+25*(pga)^2*(qga)^4*(pgb)*(qgb)^5
+25*(pga)^5*(qga)*(pgb)^4*(qgb)^2
+pga*(qga)^5*(qgb)^6
+(pga)^6*(pgb)^5*qgb