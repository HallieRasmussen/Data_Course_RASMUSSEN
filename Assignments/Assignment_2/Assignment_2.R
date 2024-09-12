#Assignment 2
getwd()

##4
csv_files = list.files(path = 'Data/',pattern = '.csv',full.names = T, recursive = T);csv_files

##5
length(csv_files)

##6
df = read.csv('Data/wingspan_vs_mass.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "");df

##7
head(df,5)

##8
b_files = list.files(path = 'Data/',pattern = '^b',full.names = T,recursive = T);b_files

##9
for (i in b_files) {
  top_line = readLines(i,n=1)
  print(top_line)
}

##10
csv_files_1 = list.files(path = 'Data/',pattern = '.csv',full.names = T, recursive = T)
for (i in csv_files_1) {
  top_line = readLines(i,n=1)
  print(top_line)
}

