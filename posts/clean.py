import os
import in_place

for root, dirs, files in os.walk(os.path.curdir):
    for f in files:
        full = os.path.join(root, f)
        if os.path.splitext(f) in ['.markdown', '.md']:
            with in_place.InPlace(full) as md:
                meta = False
                for line in md:
                    if line == "---":
                        meta = not meta
                    fields = line.split(':')
                    if meta and fields[0] == 'title' and len(fields) > 1:
                        fields[1].strip()
