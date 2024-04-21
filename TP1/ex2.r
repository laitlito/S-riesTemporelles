#je suppose etre dans le repertoire qui contient le fichier de donnees

#le fichier n'etant pas un .txt, on ne peut pas utiliser read.table
#c'est load qu'il va falloir utiliser

A=load('data1.RData')
A

#cela permet de voir que l'objet que l'on a charge se nomme khct.df

dim(khct.df)
#cela permet de voir que cet objet se compose de 4 colonnes et 180 lignes

head(khct.df)
#permet de voir le nom des 4 colonnes, a savoir kwh, htdd, cldd et t1

data=khct.df
#cela permet de renommer l'objet data

pairs(data)
#cela permet de tracer le nuage de points de chaque couple de varaibles, une variable etant une colonne de l'objet data
#on peut voir ainsi une certaine relation lineaire entre la variable kwh et t1

Y=sqrt(data$kwh)
#transformation de la variable kwh de l'objet data
#la redaction data$kwh est possible car kwh est un data.frame

#pour faire la regression lineaire multiple par nous meme, il faut creer la matrice X qui va contenir une premiere colonne ne contenant que des 1, puis les variables htdd, cldd et t1 de l'objet data

B=data.frame(int=rep(1,180),htdd=data$htdd,cldd=data$cldd,t1=data$t1)
#rep(1,180) permet de creer un vecteur ne contenant que des 1 repetes 180 et ensuite on recopie les autres variables
X=as.matrix(B)[1:168,]
Y=matrix(Y[1:168],ncol=1)
#on ne conserve que les donnees relatives aux annees 1970 a 1983 et on transforme en matrice pour pouvoir mener les calculs ensuite
#de meme on transforme Y en un vecteur colonne et on preleve les memes individus que pour X

#pour calculer les estimations des coefficients du modele, il faut calculer ce qui etait appele dans le cours beta_chapeau, soit (t(X).X)^(-1).t(X).Y

inv=solve(t(X)%*%X)
#permet de calculer l'inverse de la matrice t(X).X
#attention a l'ectriture du produit matriciel dans le logiciel

betah=inv%*%t(X)%*%Y

C=B[1:168,]
#pour utiliser la fonction lm, on extrait les donnees jusqu'a l'annee 1983
betahb=lm(Y~htdd+cldd+t1,data=C)
#Y est la variable reponse
#~est pour indiquer que la variable avant est la variable a expliquer et qu'ensuite vont apparaitre les variables explicatives
#on dit que l'on prend les variables htdd, cldd et t1 comme variables explicatives
#comme ces variables n'existent que dans l'objet C, il faut le specifier dans la fonction lm par l'argument data

betah
betahb
#on remarque que l'on a les memes valeurs

#il faut verifier le caractere centre, homoscedastique (pas besoin du caractere normal pour ce qui est fait)

plot(betahb)
#le premier graphique est celui des residus en fonction des valeurs predites.
#il faut regarder si on a plus ou moins une repartition homogene du nuage de point afin de verifier l'homoscedasticité et une repartition equitable de part et d'autre de la ligne horizontale egale a 0 pour verifier le caractere centre.
#si l'on regarde, le caractere homogene n'est pas vraiment satisfait car le nuage semble large puis se retrecir et ensuite s'elargir de nouveau.
#donc on peut remettre en cause les hypotheses.

Bb=data.frame(int=rep(1,180),htdd=data$htdd,cldd=data$cldd,t1=data$t1,t2=(data$t1-1977)^2)
Cb=Bb[1:168,]
betah2=lm(Y~htdd+cldd+t1+t2,data=Cb)

Cnew=as.matrix(Bb[169:180,])
#on recupere les donnees des variables explicatives pour l'annee 1984, et on les transforme en une matrice
mod=matrix(data=betah2$coefficients,ncol=1)
#on recupere le vecteur avec les coefficients du modele

pred=Cnew%*%mod
#on calcule les predictions associees aux differents mois de l'annee 1984
#cela est a comparer avec Ynew=sqrt(data$kwh)[169:180]

Ynew=sqrt(data$kwh)[169:180]
#puisque l'on parle de 80%, cela signifie que alpha=20% soit alpha=0.2
alpha=0.2

Int=matrix(data=0,ncol=5,nrow=12)
#12 lignes car il y a 12 mois dans une annee
Int[,1]=Ynew
#on complete la premiere colonne de Int avec les vraies valeurs de la variable reponse relatives a l'annee 1984

nl=nrow(Cb)
#nombre de donnees dans la base de donnees qui a servi a construire le modele

#il nous faut estimer la varaiance du bruit
predl=predict(betah2)
sigma2e=1/(nl-5)*sum((Y-predl)^2)

#predl est le vecteur contenant les predictions par le modele pour les annees de 1970 a 1983
#on fait nl-5 car il y a 5 variables explicatives en comptant l'intercept qui forment une famille libre

X=as.matrix(Bb[1:168,])
inv=solve(t(X)%*%X)

for (i in 1:12)
{
  Int[i,2]=pred[i,1]-sqrt(sigma2e)*sqrt(matrix(data=Cnew[i,],nrow=1)%*%inv%*%t(matrix(data=Cnew[i,],nrow=1)))*qt(1-alpha/2,nl-5)
  Int[i,3]=pred[i,1]+sqrt(sigma2e)*sqrt(matrix(data=Cnew[i,],nrow=1)%*%inv%*%t(matrix(data=Cnew[i,],nrow=1)))*qt(1-alpha/2,nl-5)
  Int[i,4]=pred[i,1]-sqrt(sigma2e)*sqrt(1+matrix(data=Cnew[i,],nrow=1)%*%inv%*%t(matrix(data=Cnew[i,],nrow=1)))*qt(1-alpha/2,nl-5)
  Int[i,5]=pred[i,1]+sqrt(sigma2e)*sqrt(1+matrix(data=Cnew[i,],nrow=1)%*%inv%*%t(matrix(data=Cnew[i,],nrow=1)))*qt(1-alpha/2,nl-5)
}
#dans la matrice Int, la premiere colonne contient donc les vraies valeurs
#les colonnes 2 et 3 contiennent respectivement les bornes inferieures et superieures de intervalles de confiance pour la moyenne
#on peut comparer cela a predict(betah2,newdata=as.data.frame(Cnew),interval='confidence',level=0.8)
#les colonnes 4 et 5 contiennent respectivement les bornes inferieures et superieures de intervalles de confiance pour la vraie valeur
#on peut comparer cela a predict(betah2,newdata=as.data.frame(Cnew),interval='prediction',level=0.8)

#les comparaisons donnent les memes valeurs.

#en ce qui concerne le trace, cela est bien evidemment impossible car il faudrait pouvoir faire un graphe en dimension 5, ce qui n'est pas possible

