setwd("/Users/apple/Box Sync/output")
list.files()


#################
###           ###
###  Hillary  ###
###           ###
#################
Aliases = read.csv('Aliases.csv')
EmailReceivers = read.csv('EmailReceivers.csv')
Emails = read.csv('Emails.csv')
Persons = read.csv('Persons.csv')

#getting table dimensions
dims = as.data.frame(rbind(

	t(c('Aliases', dim(Aliases))),

	t(c('Email Receivers', dim(EmailReceivers))),

	t(c('Emails', dim(Emails))),

	t(c('Persons', dim(Persons)))

))

names(dims) = c('Table Name', 'Rows', 'Columns')

print(dims)
###        Table Name Rows Columns
### 1         Aliases  850       3
### 2 Email Receivers 9306       3
### 3          Emails 7945      22
### 4         Persons  513       2

###################
###################
###             ### 
### Creat NODES ###
###             ### 
###################
###################### from "Persons": Id, Name [1:513]

	###################################
	#								#
	# each person frequency as Sender #
	#								#
	###################################
	sender.count = data.frame(table(Emails$SenderPersonId))
	## names(sender.count) [1] "Var1" "Freq"

	hillary_nodes = merge(Persons, sender.count, by.x="Id", by.y="Var1",all.x=T)
	
	
	hillary_nodes[is.na(hillary_nodes)] = 0  # NA: not as a sender

	names(hillary_nodes) = c("Id","Name","send_freq") 				#head(hillary_nodes)


	#####################################
	#								  #
	# each person frequency as Receiver #
	#								  #
	#####################################
	receiver.count = data.frame(table(EmailReceivers[,3]))
	## names(receiver.count) [1] "Var1" "Freq"

	hillary_nodes = merge(hillary_nodes, receiver.count, by.x="Id", by.y="Var1",all.x=T)

	hillary_nodes[is.na(hillary_nodes)] = 0 # NA: not as a receiver
	
	names(hillary_nodes) = c(names(hillary_nodes)[-4],"receive_freq") # head(hillary_nodes)

	
	#################################
	# 							  #
	# Node Size: activeness measure #
	# 							  #
	#################################
	# active_size = send_freq + receive_freq
	active_size = hillary_nodes[,3]+hillary_nodes[,4]
	
	
	
	##################################
	# 							   #
	# Node Type: @state.gov or other #
	# 							   #
	##################################
	type_id = rep(2,dim(Persons)[1]) ### type_id = 2 "@state gov" 
	hillary_nodes = data.frame(hillary_nodes,active_size,type_id)
	
	state.index = grepl("state",unlist(hillary_nodes[,2]),ignore.case=T)
	hillary_nodes$type_id[state.index==FALSE] = 1  ### other
	
		### 
		# > Persons[80,]
		#    Id            Name
		# 80 80 Hillary Clinton

		# > Persons[81,]
		#    Id        Name
		# 81 81 Huma Abedin

		# > Persons[87,]
		#    Id          Name
		# 87 87 Jake Sullivan

		# > Persons[32,]
		#    Id         Name
		# 32 32 Cheryl Mills

		table(hillary_nodes$type_id)
		#  1    2    
		#  356 157 


	########################
	# 			          #
	# Export Nodes as file #
	# 				      #
	########################
	h.nodes = data.frame(hillary_nodes$Id, hillary_nodes$Name, hillary_nodes$type_id, hillary_nodes$active_size)
	names(h.nodes) =c("id","name","person_type","active_size")
	# dim(h.nodes) 513 4 
	
	write.table(h.nodes,file="Hillary_nodes.csv",sep=",", col.names = NA, qmethod = "double")



###################
###             ### 
### Creat EDGES ###
###             ### 
###################
###################### from "EmailReceivers" 7945

	########################
	# 			          #
	# Building From & To   #
	# 				      #
	########################
	
	# names(Emails) # 'Id', 'SenderPersonId', 'ExtractedCc'
	hillary_edges = data.frame(Emails$Id, Emails$SenderPersonId, Emails$ExtractedCc)
	hillary_edges = merge(hillary_edges, EmailReceivers[-1], by.x="Emails.Id", by.y="EmailId")

	names(hillary_edges) = c("email_id", "sender_id","cc_list","receiver_id")

	## emails with NO Cc
	no.cc = nchar(as.character(hillary_edges$cc_list))

	## emails with Cc and counts
	library(stringr)
	yes.cc = str_count(hillary_edges$cc_list,";") ### ";" 
	yes.cc =  yes.cc + 1 ### no. of counts = no. of ";" + 1
	yes.cc[which(no.cc==0)] = 0  ## excluding the ones with NO Cc
	length(yes.cc)

	hillary_edges = data.frame(hillary_edges, yes.cc)
	head(hillary_edges)

	## emails with multiple receipients and counts
	no.receiver = data.frame(table(hillary_edges$email_id))
	hillary_edges = merge(hillary_edges, no.receiver, by.x="email_id",by.y="Var1")
	dim(hillary_edges) # 9306 6
	names(hillary_edges) = c(names(hillary_edges)[-6], "group.size")
	head(hillary_edges)
	dim(hillary_edges)

	type = rep(NA,dim(hillary_edges)[1])
	weight = rep(20,dim(hillary_edges)[1])

	hillary_edges = data.frame(hillary_edges, weight,type)
	head(hillary_edges)

	hillary_edges$type[which(hillary_edges$sender_id=="80")] = "sent"
	hillary_edges$type[which(hillary_edges$sender_id!="80")] = "received"
	hillary_edges$type[is.na(hillary_edges$type)] = "other"

	dettach(hillary_edges)
	hillary_edges$weight = hillary_edges$weight  - hillary_edges$yes.cc - hillary_edges$group.size


h.edges = data.frame(hillary_edges$sender_id, hillary_edges$receiver_id, hillary_edges$weight, hillary_edges$type)
names(h.edges) = c("from","to", "weight","type")
head(h.edges)
dim(h.edges) # 9306  5

test = aggregate(h.edges[,3],h.edges[,-3],sum)
test = test[order(test$from,test$to),]
colnames(test)[4] = "weight"
rownames(test) <-  NULL

library(igraph)

net = graph_from_data_frame(d=test, vertices=h.nodes,directed=T)
class(net)
E(net)$type

plot(net2, vertex.label=NA)

net2 = simplify(net, remove.multiple = F, remove.loops = T)

names(V(net2))

V(net2)$size =  (log(V(net2)$active_size)+1)/10

colrs = c("tomato","gold","gray50")
V(net2)$color = colrs[V(net2)$person_type]

E(net2)$width = log(E(net)$weight)/6

E(net2)$arrow.size = 0.2
E(net2)$edge.color = "gray80"

plot(net2)

layouts = grep("^layout_", ls("package:igraph"),value=T)[-1]
layouts = layouts[!grepl("bipartite|merge|norm|sugiyama|tree",layouts)]
# [1] "layout_as_star"       "layout_components"    "layout_in_circle"     "layout_nicely"       
# [5] "layout_on_grid"       "layout_on_sphere"     "layout_randomly"      "layout_with_dh"      
# [9] "layout_with_drl"      "layout_with_fr"       "layout_with_gem"      "layout_with_graphopt"
# [13] "layout_with_kk"       "layout_with_lgl"      "layout_with_mds"     

par(mfrow=c(3,5),mar=c(1/2,1/2,1/2,1/2))
for (layout in layouts){
	print(layout)
	l = do.call(layout,list(net2))
	plot(net2,vertex.label=NA,layout=l,main=layout)
}