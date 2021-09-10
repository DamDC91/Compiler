import random
exp= ""
t=0
max_t= random.randint(5,15)

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

def random_nb():
    nb  = random.randint(-100,100)
    if nb == 0:
        return random_nb()
    return nb

p=0
while (t<max_t):
    test = random.randint(1,20)
    if (test < 3 and (t + 2 < max_t) and False): #no (
        p=p+1
        exp = exp + " ( "

    exp = exp + " " + str(random_nb())

    test = random.randint(1,20)
    if (test < 5 and p>0):
        p=p-1
        exp = exp + " ) "

    exp = exp + " " + random_op()

    test = random.randint(1,20)
    if (test < 3 and (t + 2 < max_t)):
        exp = exp + " " + str(random_nb())
        op = random.randint(1,8)
        if (op==1):
            exp = exp + " && "
        elif (op==2):
            exp = exp + " || "
        elif (op==3):
            exp = exp + " == "
        elif (op==4):
            exp = exp + " != "
        elif (op==5):
            exp = exp + " < "
        elif (op==6):
            exp = exp + " <= "
        elif (op==7):
            exp = exp + " > "
        elif (op==8):
            exp = exp + " >= "
    
    if (t + 1 == max_t):
        exp = exp + " " + str(random_nb())

    t=t+1

for i in range(0,p):
    exp = exp + " ) "

#my C
f=open("test1.c", "w")
f.write("{ debug " + exp + "; }")
f.close()

#real C
f=open("test1_1.c", "w")
f.write("#include \"stdio.h\"\n")
f.write("int main() {\n")
f.write("printf(\"%d\\n\", " + exp + ");\n")
f.write("}\n")
f.close()
