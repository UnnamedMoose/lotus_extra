# -*- coding: utf-8 -*-
"""
Created on Sun Aug 18 11:27:21 2019

@author: Artur Lidtke (a.lidtke@marin.nl)
"""

# because we live in a Python-3 world now
from __future__ import absolute_import, division, print_function, unicode_literals

import numpy as np
import yaml

data = {
	"float": 40.0,
	"int": 9000,
	"vec": np.array([0.3456, 0, 0.5])
}

# Convert vectors to their string representation - if read into Python again, these will need to be unwrapped from strings.
for k in data:
    try:
        data[k] = " ".join([str(v) for v in data[k]])
    except TypeError:
        pass

# Write to yaml
with open("./testData/controlInputs.dat", "w") as f:
	yaml.dump(data, f, default_flow_style=False)
