import random
exp= ""
t=0
max_t= random.randint(5,10)

def random_op():
    op = random.randint(1,5);
    if (op==1):
        return "+"
    if (op==2):
        return "-"
    if (op==3):
        return "*"
    if (op==4):
        return "/"
    if (op==5):
        return "%"

while (t<max_t):
    exp = exp + " " + str(random.randint(-100,100))
    exp = exp + " " + random_op()

    test = random.randint(1,10)
    if (test < 3 and (t + 2 > max_t)):
        exp = exp + " " + str(random.randint(-100,100))
        op = random.randint(1,2)
        if (op==1):
            exp = exp + " && "
        else:
            exp = exp + " || "
    
    elif (t + 1 == max_t):
        exp = exp + " " + str(random.randint(-100,100))
    t=t+1


#my C
f=open("test1.c", "w")
f.write("{ debug " + exp + "; }")
f.close()

#real C
f=open("test1_1.c", "w")
f.write("#include \"stdio.h\"\n")
f.write("int main() {\n")
f.write("printf(\"%d \\n\", " + exp + ");\n")
f.write("}\n")
f.close()
