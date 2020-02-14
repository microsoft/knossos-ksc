def specialized_functions():
    # may depend on the choice of backend in the future
    return {
        "to_float@i": "to_float_i",
        "div@ii": "div_ii",
        "div@ff": "div_ff",
        "div@vfvf": "div_ff",
        "div@vvfvvf": "div_ff",
        "div@vvvfvvvf": "div_ff",
        "div@vvvvfvvvvf": "div_ff",
        "max@ff": "max_"
    }
