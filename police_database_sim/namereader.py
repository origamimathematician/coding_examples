
import csv

def readNames(filePath):
    nameList = []
    with open(filePath, "r") as names:
        nameReader = csv.reader(names, delimiter = "\t")
        for line in nameReader:
            #print(line)
            nameList.append(line[1] + ", " + line[0])
    return nameList
    
def main():
    test = readNames("names.txt")
#    #print(test)
#    keys = []
#    for name in test:
#        key = 1
#        for char in name[0]+name[1]:
#            key += ord(char)
#        key *= len(name)
#        keys.append(key)
#        #print(key)
#    print(keys)
#    for key in keys:
#        #print(keys.index(key))
#        #print("here" + keys[:keys.index(key)])
#        for check in keys[:keys.index(key)]:
#            if check == key:
#                print("match")
#                print(str(keys.index(key)) + " and " + str(keys.index(check)))
#        for check in keys[keys.index(key)+1:]:
#            if check == key:
#                print("match")
#                print(str(keys.index(key)) + " and " + str(keys.index(check)))
    
if __name__ == "__main__":
    main()
