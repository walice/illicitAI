#!/usr/bin/env python3

import papermill as pm
import os

pm.execute_notebook("Scripts/Predictions.ipynb",
                    "Scripts/Predictions.ipynb")

quit()