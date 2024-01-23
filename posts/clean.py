import os
import in_place

for root, dirs, files in os.walk(os.path.curdir):
    for f in files:
        full = os.path.join(root, f)
        if os.path.splitext(f)[1] in ['.markdown', '.md']:
            with in_place.InPlace(full) as md:
                meta = False
                for line in md:
                    rewritten = False
                    if line == "---\n":
                        meta = not meta
                    colonidx = line.find(':')
                    if meta and colonidx != -1 and line[:colonidx] == 'title':
                        content = line[colonidx+1:].strip()
                        md.write(f'title: \'{content}\'\n')
                        rewritten = True

                    if not rewritten:
                        md.write(line)

