import os
import in_place

for root, dirs, files in os.walk(os.path.curdir):
    for f in files:
        full = os.path.join(root, f)
        if os.path.splitext(f)[1] in ['.markdown', '.md']:
            with in_place.InPlace(full) as md:
                meta = False
                for line in md:
                    if line == "---\n":
                        meta = not meta
                    colonidx = line.find(':')
                    if meta and colonidx != -1 and line[:colonidx] == 'title':
                        content = line[colonidx+1:].strip()
                        if content[0] != '\'' and content[0] != '"':
                            if "'" in content and '"' in content:
                                print(f"Bad title! {full}")
                            elif "'" in content:
                                content = '"' + content + '"'
                            else:
                                content = "'" + content + "'"
                        line = f'title: {content}\n'

                    if not meta:
                        line = line.replace('$latex ', '$')

                    md.write(line)

