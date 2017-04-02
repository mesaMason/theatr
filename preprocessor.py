#!/usr/bin/python3

import sys
import re


def insert(s, char, p):
    if (p > len(s)+1 or p < 0):
        raise ValueError("p can't be larger that string length {}".format(len(s)))
    else:
        if p == 0:
            return char+s
        else:
            return s[:p]+char+s[p:]

def insert_mult_char(s, char_pos):
    for i, a in enumerate(char_pos):
        s = insert(s, a[0], a[1]+i)
    return s

def update_stack(stack, new_point, result):
    stack_rev = reversed(stack)
    insert_lbrace_disabled = 0
    for a in stack_rev:
        # if new block is smaller or of equal size, keep deleting the blocks below 
        if a[1] > new_point[1]:
            print('insert rbrace at {}'.format(new_point[0]))
            result.append(('}', new_point[0]))
            # can't insert an lbrace once a rbrace is inserted
            insert_lbrace_disabled = 1
            del stack[-1]
        if a[1] == new_point[1]:
            insert_lbrace_disabled = 1
            del stack[-1]
        if a[1] < new_point[1]:
            # only insert next block if the previous block is one size smaller
            assert(new_point[1] == a[1] + 1)
            if not insert_lbrace_disabled:
                print('insert lbrace at {}'.format(new_point[0]))
                result.append(('{', new_point[0]))
            break;
    stack.append(new_point)
    print(stack)
    print("\n")

def preprocess(filename):
	f = open(filename,'r')
	content = f.read()
	print(content)
	f.close()
        # insert semicolon at the end of last statementin each line
	p = re.compile("\n *")
	stack = [('dummy',0)]
	tab = 4
	result = []
	for a in p.finditer(content):
		assert((len(a.group())-1) % tab == 0)
		new_point = (a.start(), (len(a.group())-1)//tab)
		update_stack(stack, new_point, result)
        # to ensure that for the stack, the file ends with a new line
        update_stack(stack, ('dummy', 0), result)
	f = open('filename'+'.out', 'w')
	print(insert_mult_char(content, result))
	f.write(insert_mult_char(content, result))
	f.close()

files = sys.argv
for filename in files[1:]:
	preprocess(filename)
