#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 23 19:09:33 2019

@author: Bruno Riccelli
"""

import pandas as pd
import numpy as np

# Read dataset
df = pd.read_csv('/home/latin/R/ISCX2017/MachineLearningCVE/CICISCX2017CleanedPCATransformed.csv')

# Display example observations
df.head()
#Convertendo o nome das colunas para lower case e removendo os espaços
df.columns = df.columns.str.strip().str.lower().str.replace(' ', '_').str.replace('(', '').str.replace(')', '')

# Pegando array com os nomes das classes de trafego
classesNames = df['dfiscx.label'].unique()
# quantidade de cada classe
classesCounts = df['dfiscx.label'].value_counts()
classesCounts2 = df['dfiscx.label'].value_counts()
# Removendo coluna criada pelo R
df = df.drop("unnamed:_0", axis=1)

#transformando a coluna dfiscx.label para categorical
df['dfiscx.label'] = df['dfiscx.label'].astype('category')
# transformando categorical em numerical
df['dfiscx.label'] = df['dfiscx.label'].cat.codes

# Pegando os tamanhos das classes
#BENIGN
minority_class0_len = len(df[df['dfiscx.label'] == 0])
print(minority_class0_len)
#BOT
minority_class1_len = len(df[df['dfiscx.label'] == 1])
print(minority_class1_len)
#DDOS
minority_class2_len = len(df[df['dfiscx.label'] == 2])
print(minority_class2_len)
#Dos GoldenEye
minority_class3_len = len(df[df['dfiscx.label'] == 3])
print(minority_class3_len)
#DOS HULK
minority_class4_len = len(df[df['dfiscx.label'] == 4])
print(minority_class4_len)
#DoS Slowhttptest
minority_class5_len = len(df[df['dfiscx.label'] == 5])
print(minority_class5_len)
#FTP-Patator
minority_class6_len = len(df[df['dfiscx.label'] == 6])
print(minority_class6_len)
#SSH-Patator
minority_class7_len = len(df[df['dfiscx.label'] == 7])
print(minority_class7_len)
#Heartbleed
minority_class8_len = len(df[df['dfiscx.label'] == 8])
print(minority_class8_len)
#Infiltration
minority_class9_len = len(df[df['dfiscx.label'] == 9])
print(minority_class9_len)
#PortScan
minority_class10_len = len(df[df['dfiscx.label'] == 10])
print(minority_class10_len)
#SSH-Patator
minority_class11_len = len(df[df['dfiscx.label'] == 11])
print(minority_class11_len)
#Web Attack Brute Force
minority_class12_len = len(df[df['dfiscx.label'] == 12])
print(minority_class12_len)
#Web Attack SQL Injection
minority_class13_len = len(df[df['dfiscx.label'] == 13])
print(minority_class13_len)
#Web Attack XSS
minority_class14_len = len(df[df['dfiscx.label'] == 14])
print(minority_class14_len)

#Total batendo com size
total =  minority_class0_len + minority_class1_len + minority_class2_len + minority_class3_len + minority_class4_len+ minority_class5_len+ minority_class6_len+ minority_class7_len+ minority_class8_len+ minority_class9_len+ minority_class10_len+ minority_class11_len+ minority_class12_len+ minority_class13_len+ minority_class14_len
dfSize = len(df)

# Definindo os tamanhos de cada classe de acordo com a proporçao desejada do dataset 
#BENIGN
size0 = 25448
#BOT
size1 = 1956
#DDOS
size2 = 25447
#DOS GOLDENEYE
size3 = 10293
#DOS HULK
size4 = 25448
#DOS SLOWHTTP
size5 = 5499
#DOS SLOWRIS
size6 =5796
#FTP-PATATOR
size7 = 7935
#heARTBLEED
size8 = 11
#INFILTRATION
size9 = 36
# PORTSCAN
size10 = 25448
#SSH-PATATOR
size11 = 5897
 #BRUTE FORCE
size12 = 1507
#SQL INJECTION
size13 = 21
#XSS
size14 = 652
teste= size0+size1+size2+size3+size4+size5+size6+size7+size8+size9+size10+size11+size12+size13+size14

# Realizando a subamostragem
from imblearn.under_sampling import RandomUnderSampler
strategy = {0:size0, 1:size1, 2: size2, 3: size3, 4:size4,5:size5,6:size6,7:size7,8:size8,9:size9,10:size10,11:size11,12:size12,13:size13,14:size14}
ros = RandomUnderSampler(sampling_strategy=strategy, random_state= 7)
X_under, y_under = ros.fit_resample(df, df['dfiscx.label'])

# transformando os vetores em dataframes
X_under = pd.DataFrame(X_under)
y_under = pd.DataFrame(y_under)

# Concatenando features e classes 
dataset = pd.concat([X_under, y_under])
# salvando em CSV
export_csv = dataset.to_csv (r'/home/latin/export_dataframe5PercentCleanedPCA.csv', index = None, header=True) #Don't forget to add '.csv' at the end of the path
