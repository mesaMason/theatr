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
    """	
    	Each entry in the stack is (a = new_line_character_position, b = number of tabs following new line)
    	Iterate through the stack in reverse order. The stack always 
    	has bigger blocks above smaller block. The order is defined by b. 

    	INPUT: stack, new stack entry, a list where result would be appended
    	result is updated with tuples ('{' or '}', position where brace should be inserted)
    """
    stack_rev = reversed(stack)
    insert_lbrace_disabled = 0
    for a in stack_rev:
        # if new block is smaller or of equal size, keep deleting the blocks below 
        if a[1] > new_point[1]:
            # print('insert rbrace at {}'.format(new_point[0]))
            result.append(('}', new_point[0]))
            # can't insert a lbrace once a rbrace is inserted
            insert_lbrace_disabled = 1
            del stack[-1]
        if a[1] == new_point[1]:
            # can't insert a lbrace once a rbrace is inserted
            insert_lbrace_disabled = 1
            del stack[-1]
        if a[1] < new_point[1]:
            # only insert next block if the previous block is one size smaller
            assert(new_point[1] == a[1] + 1)
            if not insert_lbrace_disabled:
                # print('insert lbrace at {}'.format(new_point[0]))
                result.append(('{', new_point[0]))
            break;
    stack.append(new_point)
    # print(stack)
    # print("\n")

def remove_comments(content):
	def replacer(match):
		s = match.group(0)
		if s.startswith('/'):
			return " "
		else:
			return s
	comment_pattern = re.compile(
		r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',
		#r"""
        #//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"
        #""",
        re.DOTALL | re.MULTILINE
	)
	return re.sub(comment_pattern, replacer, content)

def preprocess(filename, fileout):
	"""
	input: filename to be preprocessor
	output: filename.out file with braces and semicolons inserted at the end of statement
	"""
	f = open(filename,'r')
	
	content = f.read()
	
	content = remove_comments(content)
	f.close()
	content = re.sub(r'\n\s*\n', '\n', content)
	content = re.sub(r' *\n', '\n', content)
	## insert semicolons
	# insert a new line to help with preprocesinng. this is removed before writing the result
	content = content + "\n"
	p = re.compile("[^\s:;]\n")
	result = []
	for a in p.finditer(content):
	    # print(a.end())
	    result.append((';',a.end()-1))
	content = insert_mult_char(content, result)
	
	# insert braces around conditional statements and functions.  
	p = re.compile("\n *")
	stack = [('dummy',0)]
	tab = 4
	result = []
	for a in p.finditer(content):
	    assert((len(a.group())-1) % tab == 0)
	    new_point = (a.start(), (len(a.group())-1)//tab)
	    update_stack(stack, new_point, result)
	f = open(fileout, 'w')
	content = insert_mult_char(content, result)
	content = content[:-1]
	f.write(content)
	f.close()

files = sys.argv[1:]
preprocess(files[0], files[1])
