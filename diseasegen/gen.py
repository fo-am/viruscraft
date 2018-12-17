import random

def proc(fn):
    names = []
    for line in open(fn,"r"):
        stripped = line.strip()
        bracket = stripped.find("[")
        if bracket!=-1:
            names.append(stripped[0:bracket])
        else:
            bracket = stripped.find("(")
            if bracket!=-1:
                names.append(stripped[0:bracket-1])
            else:
                names.append(stripped)
    return names

def choose(l):
    return l[random.randrange(0,len(l))]
      
def gen(names):
    print(choose(names[0])+" "+choose(names[1])+" "+choose(names[2])+" "+choose(names[3]))

def export(names):
    print("((")
    for i in names[0]: print("\""+i+"\"")
    print(") (")
    for i in names[1]: print("\""+i+"\"")
    print(") (")
    for i in names[2]: print("\""+i+"\"")
    print(") (")
    for i in names[3]: print("\""+i+"\"")
    print("))")


def size(names):
    print(len(names[0])*len(names[1])*len(names[2])*len(names[3]))

names = [proc("places.txt"),proc("parts.txt"),proc("symptom.txt"),proc("desc.txt")]

size(names)

for i in range(0,10):
    gen(names)

export(names)


