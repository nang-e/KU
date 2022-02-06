#!/usr/bin/env python
# coding: utf-8

# In[2]:


a,b='',''
a=input('문자열을 입력하세요--> ')
for x in a:
    if x.isdigit():
        continue
    b+=x
    
print('숫자 제거--> '+b)

