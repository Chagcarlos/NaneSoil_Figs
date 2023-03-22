#install.packages("soiltexture")
#install.packages("readxl")
#install.packages("devEMF")

library(soiltexture)
library(readxl)
library(devEMF)
library(ggplot2)

database=read_xlsx("C:/Users/Huawei-Fuentes/OneDrive - UAQ/NATURE 2023/NaneSoil.xlsx",col_names = TRUE) #Change file path
attach(database)

database=data.frame(database)
database$Texture <- as.factor(database$Texture) 

data=database[,c(5,6,7,9,10,11,12,13)]
lab1= c("SAND\n(%)", "CLAY\n(%)", "SILT\n(%)", "BD\n(Mg/m³)","θs\n(cm³/cm³)","FC\n(cm³/cm³)","PWP\n(cm³/cm³)","Ks\n(cm/h)")
colnames(data)=lab1

TextureDB=database[,c(6,7,5)]
colnames(TextureDB) = c('CLAY','SILT','SAND')

emf(file = "Triangle.emf",width = 12,height = 12) #for create a EMF file 
TT.plot(class.sys="USDA.TT",
        tri.data = TextureDB,cex = 0.9,pch = 16,col = "firebrick2",
        cex.axis = 1.5,
        cex.lab = 1.5 ,
        class.p.bg.col =palette(hcl.colors(12, palette = "Earth")),
        class.line.col = "black")
dev.off()

emf(file = "Figure3-1.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Sand (%)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +   # Transparencia del color
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10,face = "bold"))+
  xlim(0,100)+ylim(0,0.04)
dev.off()

emf(file = "Figure3-2.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Clay (%)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))+
  xlim(0,100)+  ylim(0,0.04)
dev.off()

emf(file = "Figure3-3.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Silt (%)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))+
  xlim(0,100)+  ylim(0,0.04)
dev.off()

emf(file = "Figure3-4.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x=`Bulk Density (Mg/m³)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +   # Transparencia del color
  scale_x_continuous(name=expression(bold("Bulk Density (Mg/m"^3*")")),
                     limits = c(0.9,1.7),breaks = seq(0.9,1.7,0.2)) +ylim(0,3.5)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))
dev.off()

emf(file = "Figure3-5.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Saturation Moisture Content (cm³/cm³)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  scale_x_continuous(name=expression(bold("θ"["s"]*" (cm"^3*"/cm"^3*")")),
                     limits = c(0.3,0.6),breaks = seq(0.3,0.6,0.1))+ylim(0,11.5)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))
dev.off()

emf(file = "Figure3-6.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Field Capacity (cm³/cm³)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  scale_x_continuous(name=expression(bold("Field Capacity"*" (cm"^3*"/cm"^3*")")),
                     limits = c(0.1,0.6),breaks = seq(0.1,0.6,0.1))+ylim(0,11.5)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))
dev.off()

emf(file = "Figure3-7.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Permanent Wilting Point (cm³/cm³)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  scale_x_continuous(name=expression(bold("PWP"*" (cm"^3*"/cm"^3*")")),
                     limits = c(0,0.4))+ylim(0,11.5)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))
dev.off()

emf(file = "Figure3-8.emf",width = 6,height = 6, units = "cm")
ggplot(data, aes(x = `Ks (cm/h)`)) + 
  geom_density(color = "midnightblue", # Color de la curva
               fill = "skyblue",  # Color del área sombreada
               alpha = 0.5) +  # Transparencia del color
  scale_x_continuous(name=expression(bold("K"["s"]*" (cm/h)")),
                     limits = c(0,5.5),breaks = seq(0,6,1))+ylim(0,0.8)+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10,face = "bold"))
dev.off()

emf(file = "Ks vs clay.emf",width = 14.12,height = 8)
par(mar=c(4,5,1,1)) #Ajusta el margen de la gráfica
plot(`Clay (%)`,`Ks (cm/h)`, las = 1,
     xlab = expression(bold("Clay"*" (%)")),
     ylab = expression(bold(K[s]*" (cm/h)")),
     cex.lab=2,col="steelblue4",
     xlim=c(0,70),ylim = c(0,6)) #pch=simbolos
box(lwd=3) #para remarcar el marco
panel.first=grid(col='honeydew3',lty=2) #el grid de adentro
text(55,4.5,bquote(bold(a*")")),pos=4,cex=1.5)
dev.off()

emf(file = "Smoisture vs clay.emf",width = 9,height = 6)
par(mar=c(4,5,1,1)) #Ajusta el margen de la gráfica
plot(`Clay (%)`,`Saturation Moisture Content (cm³/cm³)`, las = 1,
     xlab = expression(bold("Clay"*" (%)")),
     ylab = expression(bold("θ"["s"]*" (cm"^3*"/cm"^3*")")),
     cex.lab=1.2,col="steelblue4",
     xlim=c(0,70),ylim = c(0.30,0.60)) #pch=simbolos
box(lwd=3) #para remarcar el marco
panel.first=grid(col='honeydew3',lty=2) #el grid de adentro
text(55,0.425,bquote(bold(b*")")),pos=4,cex=1.5)
dev.off()