def main():
  with open("inputs/valid.txt", "w") as out:
    for i in range(10, 501, 5):
      with open(f"inputs/{i}.in", "r") as fin:
        out.write(str(int(fin.readline()) * int(fin.readline())) + '\n')

if __name__ == '__main__':
  main()