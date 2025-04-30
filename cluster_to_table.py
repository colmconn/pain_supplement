#!/usr/bin/python

import optparse, os, re, subprocess, sys

def setup_cmd_line_opts(args):
    parser = optparse.OptionParser()
    parser.add_option('-c', '--com',
                      dest='com',
                      default=True,
                      action='store_true',
                      help='Use the centers of mass of the clusters when interrogating whereami atlases')
    parser.add_option('-m', '--mi',
                      dest='mi',
                      default=False,
                      action='store_true',
                      help='Use the centers of maximum intensity of the clusters when interrogating whereami atlases')
    parser.add_option('-v', '--verbose',
                      dest='verbose',
                      action="store_true",
                      default=False,
                      help='Verbose output.')

    options, args = parser.parse_args()

    return(options, args)

def check_args(options, args):
    if len(args) == 0:
        print("No cluster location files provided on command line. Cannot continue", file=sys.stderr)
        sys.exit(1)

    if options.verbose:
        print(f"com   => {options.com}", file=sys.stderr)
        print(f"mi    => {options.mi}", file=sys.stderr)
        for ii, arg in enumerate(args):
            print(f"{ii:02} -> {arg}", file=sys.stderr)


def process_cluster_table(options, table_filename):
    record_sep="+++++++ nearby Atlas structures +++++++\n";
    locations=[]
    if options.com:
        columns="1,2,3"
    else:
        columns="13,14,15"

    names_filename=table_filename.replace("table", "names").replace(".txt", ".csv")
        
    #atlas="(?P<atlas>[-_a-zA-Z0-9]+)"
    atlas=r"(?P<atlas>CA_ML_18_MNI)"
    space=r"\s+"
    within=r"(?P<within>\d+(?:.\d+))"
    label=r"(?P<label>[-_a-zA-Z0-9)(]+)"
    prob=r"(?P<prob>[-_a-zA-Z0-9]+)"
    code=r"(?P<code>\d+)"
    atlas_line_re=re.compile(atlas + space + within + space + label +
                             space + prob  + space  + code,
                             flags=re.A)
    new_env=os.environ
    new_env['AFNI_WHEREAMI_NO_WARN']='y'

    result = subprocess.run(['whereami',
                             "-space",  "MNI", # options.space,
                             "-coord_file", table_filename + '[' + columns + ']',
                             "-tab"],
                            env=new_env,
                            encoding="UTF-8",
                            capture_output=True,
                            text=True)
    atlas_labels=[]
    if result.returncode == 0:
        if options.verbose:
            print("Command executed successfully:", file=sys.stderr)
        
        result.stdout=result.stdout.removeprefix('++ Input coordinates orientation set by default rules to RAI\n').strip()
        cluster_count=0
        records=result.stdout.split(record_sep)
        for record in records:
            if len(record) == 0:
                ## I have no idea why a line with 0 characters remains
                ## so just work around it.
                continue
            record=record.splitlines()
            line_count=0
            label_str=None
            for line in record:
                #print(line)
                line_count=line_count + 1
                match=re.match(atlas_line_re, line)
                if match:
                    label_str=match.group('label').replace('_', ' ')
                    # print(f"First atlas line: {cluster_count-1:03d} | {line_count:03d} | {label_str}")
                    break
            if label_str == None:
                atlas_labels.append("No label")
            else:
                atlas_labels.append(label_str)
                
            cluster_count=cluster_count+1

        if options.verbose:          
            print(f"Got {cluster_count:03d} atlas entries", file=sys.stderr)
            print(",".join(atlas_labels), file=sys.stderr)
            
        with open(names_filename, 'w') as ff:
            print(f"Processing the clusters in {table_filename} -> {names_filename}")
            ff.write(",".join(atlas_labels) + '\n')
        
    else:
        print("whereami command failed with error code:", result.returncode)
        print("Stderr:", result.stderr)
        sys.exit(1)
        
def main(args):
    options, args = setup_cmd_line_opts(args)
    check_args(options, args)

    for cluster_file in args:
        process_cluster_table(options, cluster_file)
        # sys.exit(1)

if __name__ == "__main__":
    main(sys.argv[1:])

