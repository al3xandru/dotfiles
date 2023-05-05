#!/usr/bin/env python
import random
import sys


RECIPE_CODES = ('ERX', 'EHX', 'EWX', 'ERC', 'EHC', 'EWC',
    'XEC', 'XER', 'XEH', 'XEW',
    'EKXR', 'EKXH', 'EKXW')

RECIPE_DETAILS = {
    'ERX': ('Recipe 1 (var. 111)', '2pm: Eat (1h)|3pm: Read (1h)|4pm: Exercise (1.5h)', '5:30PM', '2.5'), 
    'EHX': ('Recipe 1 (var. 121)', '2pm: Eat (1h)|3pm: Hoby (1h)|4pm: Exercise (1.5h)', '5:30PM', '2.5'), 
    'EWX': ('Recipe 1 (var. 131)', '2pm: Eat (1h)|3pm: Write (1h)|4pm: Exercise (1.5h)', '5:30PM', '2.5'), 
    'ERC': ('Recipe 1 (var. 112)', '2pm: Eat (1h)|3pm: Read (1h)|4pm: Code (2h)', '6PM', '3'), 
    'EHC': ('Recipe 1 (var. 122)', '2pm: Eat (1h)|3pm: Hoby (1h)|4pm: Code (2h)', '6PM', '3'), 
    'EWC': ('Recipe 1 (var. 132)', '2pm: Eat (1h)|3pm: Write (1h)|4pm: Code (2h)', '6PM', '3'), 
    'XEC': ('Recipe 2 (var. 211)', '2pm: Exercise (1.5h)|4pm: Eat (1h)|5pm: Code (2h)', '7PM', '3.5'), 
    'XER': ('Recipe 2 (var. 212)', '2pm: Exercise (1.5h)|4pm: Eat (1h)|5pm: Read (1h)', '6PM', '2.5'), 
    'XEH': ('Recipe 2 (var. 213)', '2pm: Exercise (1.5h)|4pm: Eat (1h)|5pm: Hoby (1h)', '6PM', '2.5'), 
    'XEW': ('Recipe 2 (var. 214)', '2pm: Exercise (1.5h)|4pm: Eat (1h)|5pm: Write (1h)', '6PM', '2.5'), 
    'EKXR': ('Recipe 3 (var. 3111)', '2pm: Eat (1h)|3pm: Work|5:30pm: Exercise (1.5h)|7:30pm: Read (1h)', '8:30PM', '2.5'), 
    'EKXH': ('Recipe 3 (var. 3112)', '2pm: Eat (1h)|3pm: Work|5:30pm: Exercise (1.5h)|7:30pm: Hoby (1h)', '8:30PM', '2.5'), 
    'EKXW': ('Recipe 3 (var. 3113)', '2pm: Eat (1h)|3pm: Work|5:30pm: Exercise (1.5h)|7:30pm: Write (1h)', '8:30PM', '2.5')   
}

ACTIVITIES = {
    'E': 'Eat',
    'R': 'Read',
    'H': 'Hoby',
    'W': 'Write',
    'X': 'Exercise',
    'C': 'Code',
    'K': 'Work'
}

def main():
    recipe_code = RECIPE_CODES[random.randint(0, len(RECIPE_CODES) - 1)]
    # recipe_code = RECIPE_CODES[0]
    recipe = ', '.join([ACTIVITIES[c] for c in recipe_code])
    details = RECIPE_DETAILS[recipe_code]
    recipe_txt = "%s: %s" % (details[0], recipe)
    print("")
    print("*" * 60)
    print("*" + "Selected activities".center(58) + "*")
    print("*" + " " * 58 + "*")
    print("*" + recipe_txt.center(58) + "*")
    print("*" + " " * 58 + "*")
    print("*" * 60)
    print('\nRecommended agenda (total time: ' + details[3] + 'h, done by ' + details[2] + '):')
    for l in details[1].split('|'):
        print(" - " + l)
    print('')


def options():
    for code in RECIPE_CODES:
        recipe = ', '.join([ACTIVITIES[c] for c in code])
        details = RECIPE_DETAILS[code]
        recipe_txt = "%s: %s (total time: %sh)" % (details[0], recipe, details[3])
        print(recipe_txt)



if __name__ == '__main__':
    if len(sys.argv) > 1:
        options()
    else:
        main()