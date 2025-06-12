from __future__ import print_function
import os, sys, shutil
from collections import defaultdict

from PIL import Image
from PIL import ExifTags

def move(f, date_created, dest_dir, dry_run=True):
    year, month = tuple(date_created.split(':')[:2])
    year, month = year.strip(), month.strip()
    if year in [None, ''] or month in [None, '']:
        print("{} date format invalid {}".format(f, date_created))
        return
    
    dest_dir = os.path.join(dest_dir, os.path.join(year, month))
    dest_f = os.path.join(dest_dir, os.path.basename(f))
    if dry_run:
        # print("Moving {} date={} to {}".format(f, date_created, dest_f))
        pass
    else:
        try:
            if not os.path.exists(dest_dir):
                # print("Making director {}".format(dest_dir))
                os.makedirs(dest_dir)
            shutil.move(f, dest_dir)
            # print("Moved {} date={} to {}".format(f, date_created, dest_f))
        except:
            print(sys.exc_info())
            sys.exit(-1)
    

LIMIT = None

def main():
    k = 0
    src=sys.argv[1]
    dest=sys.argv[2]
    counts = defaultdict(lambda: 0)

    for root, dirs, files in os.walk(src):
        for file in files:
            k = k+1
            try:
                file = os.path.join(root, file)
                im = Image.open(file)
                date_created = im._getexif().get(36867, None)
                if date_created is not None:
                    counts["dated"] = counts["dated"] + 1
                    move(file, date_created, dest, dry_run=False)
                else:
                    counts["no-date"] = counts["no-date"] + 1
                    print("{} no-date".format(file))

                if LIMIT is not None and k > LIMIT:
                    break
            except:
                counts["failed"] = counts["failed"] + 1
                print("{} failed".format(file))

    print("Counts {}".format(counts))
