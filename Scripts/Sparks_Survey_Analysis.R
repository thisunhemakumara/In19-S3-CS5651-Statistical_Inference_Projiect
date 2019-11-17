# Set Working Directory
setwd("C:/Users/User/Downloads")

# Function to Generate Pie Charts
draw.pie = function(c) {
  # Create data for the graph.
  x <- table(c)
  y <- names(x)
  z <- gsub("\\.", "_", colnames(c))
  w <- gsub("\\.", " ", colnames(c))
  print(paste("Generating Pie Chart for", z, sep=" "))
  
  # Give the chart file a name.
  png(file = paste(z, "jpg", sep="."))
  
  plot(length(z), length(z))
  
  # Plot the chart with title and rainbow color pallet.
  pie(x, labels = paste(round(prop.table(x)*100), "%", sep = ""), 
      main = w,col = rainbow(length(x)), clockwise=TRUE, radius=0.75*length(z))
  
  legend("bottomright", legend=y, col=rainbow(length(x)), 
         pch=19, cex=.8, box.col="darkgreen")
  
  # Save the file.
  dev.off()
  
}

# Function to Generate Bar Charts
draw.bar = function(c) {
  # Create data for the graph.
  x <- table(c)
  y <- names(x)
  z <- gsub("\\.", "_", colnames(c))
  w <- gsub("\\.", " ", colnames(c))
  print(paste("Generating Bar Chart for", z, sep=" "))
  
  # Give the chart file a name.
  png(file = paste(z, "jpg", sep="."))
  
  par(mar=c(4,16,4,4))
  
  # Plot the bar chart 
  barplot(height=x, names=y, 
          col="#69b3a2", main=w, 
          horiz=T, las=1
  )
  
  # Save the file.
  dev.off()
  
}

# Function to Generate Stacked Bar Charts
draw.stkbar = function(c, d) {
  # Create data for the graph.
  x <- table(d,c)
  #y <- names(x)
  z <- gsub("\\.", "_", colnames(c))
  w <- gsub("\\.", " ", colnames(c))
  q <- gsub("\\.", "_", colnames(d))
  r <- gsub("\\.", " ", colnames(d))
  y <- paste(z, q, sep="_vs_")
  s <- paste(w, r, sep="_vs_")
  mtrx <- as.matrix(x, rownames=TRUE)
  print(paste("Generating Stacked Bar Chart for", y, sep=" "))
  
  # Give the chart file a name.
  png(file = paste(y, "jpg", sep="."))
  
  #mtrx <- as.matrix(table(data$What.ethnicity.do.you.belong.to..,data$Whom.will.you.vote.if.the.presidential.election.is.held.today.), rownames=TRUE)
  
  par(mar=c(4,12,4,4))
  
  # Plot the stacked barplot
  barplot(mtrx, 
          col=colors()[c(15,30,45,60,75,90)] , 
          border="white", 
          space=0.04, 
          font.axis=2, 
          horiz=T, 
          las=1, legend = TRUE , args.legend = list(bty = "n", x = "top", ncol = 3), ylim = c(0,8))
  
  
  # Save the file.
  dev.off()
  
}

# Function to Generate Histogram with Boxplot
draw.hist = function(d, c, a, b) {
  # Create data for the graph.
  x <- c
  #y <- names(x)
  z <- gsub("\\.", "_", colnames(d))
  w <- gsub("\\.", " ", colnames(d))
  print(paste("Generating Histogram for", z, sep=" "))
  
  # Give the chart file a name.
  png(file = paste(z, "jpg", sep="."))
  
  # Plot the bar chart 

  # Layout to split the screen
  layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
  
  # Draw the boxplot and the histogram 
  par(mar=c(0, 3.1, 1.1, 2.1))
  boxplot(x , horizontal=TRUE , ylim=c(a,b), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
  par(mar=c(4, 3.1, 1.1, 2.1))
  hist(x , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main=w , xlab=w, xlim=c(a,b))
  
  # Save the file.
  dev.off()
  
}

# Read CSV 
data<- read.csv(file="C:/Users/User/Downloads/Sparks_Survey_Responses.csv", header=TRUE, sep=",")
names(data)
str(data)
summary(data)

draw.pie(data[2])
draw.pie(data[3])
draw.pie(data[4])
draw.pie(data[7])
draw.pie(data[9])
draw.pie(data[10])
draw.pie(data[13])
draw.bar(data[6])
draw.bar(data[11])
draw.bar(data[12])
draw.bar(data[15])
draw.hist(data[8], as.integer(data$Age), min(as.integer(data$Age)), max(as.integer(data$Age)))
#draw.stkbar(data[4],data[13])

mtrx <- as.matrix(table(data$Gender,data$Whom.will.you.vote.if.the.presidential.election.is.held.today.), rownames=TRUE)

# Give the chart file a name.
png(file = paste("Candidate_vs_Gender", "jpg", sep="."))

par(mar=c(4,16,4,4))

# Get the stacked barplot
barplot(mtrx, 
        col=colors()[c(16,32,48,64,80,96)] , 
        border="white", 
        space=0.04, 
        font.axis=2, 
        horiz=T, 
        las=1, legend = TRUE , args.legend = list(bty = "n", x = "top", ncol = 3), ylim = c(0,8))

# Save the file.
dev.off()
