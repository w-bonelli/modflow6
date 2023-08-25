import os
from typing import Union

import flopy
import numpy as np
import pandas as pd


def get_track_dtype(path: os.PathLike):
    """Get the dtype of the track data recarray from the ascii header file."""

    hdr_lns = open(path).readlines()
    hdr_lns_spl = [[ll.strip() for ll in l.split(",")] for l in hdr_lns]
    return np.dtype(list(zip(hdr_lns_spl[0], hdr_lns_spl[1])))


def check_track_data(
    track_bin: os.PathLike,
    track_hdr: os.PathLike,
    track_csv: os.PathLike,
):
    """Check that track data written to binary, CSV, and budget files are equal."""

    # get dtype from ascii header file
    dt = get_track_dtype(track_hdr)

    # read output files
    data_bin = np.fromfile(track_bin, dtype=dt)
    data_csv = np.genfromtxt(track_csv, dtype=dt, delimiter=",", names=True)
    if len(data_csv.shape) == 0:
        # https://stackoverflow.com/a/24943993/6514033
        data_csv = np.array([data_csv])

    assert (
        data_bin.shape == data_csv.shape
    ), f"Binary and CSV track data shapes do not match: {data_bin.shape} != {data_csv.shape}"

    # check particle tracks written to all output files are equal
    # check each column separately to avoid:
    # TypeError: The DType <class 'numpy._FloatAbstractDType'> could not be promoted by <class 'numpy.dtype[void]'>
    for k in data_bin.dtype.names:
        if k == 'name':
            continue
        assert np.allclose(data_bin[k], data_csv[k], equal_nan=True)

    # make sure columns all have values in the expected range
    assert all(data_bin["iprp"] >= 1)
    assert all(data_bin["irpt"] >= 1)
    assert all(data_bin["kper"] >= 1)
    assert all(data_bin["kstp"] >= 1)
    assert all(data_bin["ilay"] >= 1)
    assert all(data_bin["icell"] >= 1)
    assert all(data_bin["istatus"] >= 0)
    assert all(data_bin["ireason"] >= 0)


def to_mp7_format(data: Union[pd.DataFrame, np.recarray]) -> pd.DataFrame:
    if isinstance(data, pd.DataFrame):
        data = data.to_records(index=False)

    mp7_dtypes = np.dtype(
        [
            ("particleid", np.int32),
            ("particlegroup", np.int32),
            ("sequencenumber", np.int32),
            ("particleidloc", np.int32),
            ("time", np.float32),
            ("x", np.float32),
            ("y", np.float32),
            ("z", np.float32),
            ("k", np.int32),
            ("node", np.int32),
            ("xloc", np.float32),
            ("yloc", np.float32),
            ("zloc", np.float32),
            ("stressperiod", np.int32),
            ("timestep", np.int32),
        ]
    )

    return pd.DataFrame(
        np.core.records.fromarrays(
            [
                data["irpt"],
                data["iprp"],
                np.zeros(
                    data.shape[0]
                ),  # todo use sequencenumber passed explicitly as particle name
                np.zeros(data.shape[0]),
                data["t"],
                data["x"],
                data["y"],
                data["z"],
                data["ilay"],  # todo add to PRT output?
                data["icell"],
                np.zeros(data.shape[0]),
                np.zeros(data.shape[0]),
                np.zeros(data.shape[0]),
                data["kper"],
                data["kstp"],
            ],
            dtype=mp7_dtypes,
        )
    )


def check_budget_data(lst: os.PathLike, perlen, nper):
    # load PRT model's list file
    mflist = flopy.utils.mflistfile.ListBudget(
        lst, budgetkey="MASS BUDGET FOR ENTIRE MODEL"
    )
    names = mflist.get_record_names()
    entries = mflist.entries

    # check timesteps
    inc = mflist.get_incremental()
    v = inc["totim"][-1]
    assert v == perlen * nper, f"Last time should be {perlen}.  Found {v}"

    # entries should be a subset of names
    assert all(e in names for e in entries)

    # todo what other record names should we expect?
    expected_entries = [
        "PRP_IN",
        "PRP_OUT",
    ]
    assert all(en in names for en in expected_entries)


def get_output_event(case_name):
    return (
        "ALL"
        if "all" in case_name
        else "RELEASE"
        if "rel" in case_name
        else "TRANSIT"
        if "trst" in case_name
        else "TIMESTEP"
        if "tstp" in case_name
        else "WEAKSINK"
        if "wksk" in case_name
        else "TERMINATE"
        if "terminate" in case_name
        else "ALL" # default
    )


def get_ireason_code(output_event):
    return (
        0 if output_event == "RELEASE"
        else 1 if output_event == "TRANSIT"
        else 2 if output_event == "TIMESTEP"
        else 3 if output_event == "TERMINATE"
        else 4 if output_event == "WEAKSINK"
        else -1 # default
    )


def all_equal(col, val):
    a = col.to_numpy()
    return a[0] == val and (a[0] == a).all()


def has_default_boundnames(data):
    name = [int(n.rpartition("0")[2]) for n in data["name"].to_numpy()]
    irpt = data["irpt"].to_numpy()
    return np.array_equal(name, irpt)
