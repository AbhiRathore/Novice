attach(CellService)
summary(CellService)
summary(T.Mobile)
summary(AT.T)
summary(Verizon)
median(T.Mobile)
median(AT.T)
median(Verizon)
plot(data.frame(CellService))
hist(CellService$AT.T)
sum(table(City))
sum(table(AT.T))
sum(table(Sprint))

AT.T[(0.9)*(sum(table(AT.T)))]

table_data_AT=by(City, INDICES=AT.T,FUN=unque)
table_data_AT
