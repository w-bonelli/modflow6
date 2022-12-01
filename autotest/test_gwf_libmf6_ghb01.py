"""
MODFLOW 6 Autotest
Test the api which is used update to set the simulate the effect of a general
head boundary (ghb) at the downgradient end of the model with a head below the
bottom of the cell. The api results are compared to a non-api simulation that
uses the well package to simulate the effect of the same ghb. This is a
possible solution to https://github.com/MODFLOW-USGS/modflow6/issues/724
"""
import os

import numpy as np
import pytest
from modflowapi import ModflowApi

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation, api_return

ex = ["libgwf_ghb01"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))


# temporal discretization
nper = 10
tdis_rc = []
for i in range(nper):
    tdis_rc.append((1.0, 1, 1))

# model spatial dimensions
nlay, nrow, ncol = 1, 1, 10

# cell spacing
delr = 50.0
delc = 1.0
area = delr * delc

# top of the aquifer
top = 25.0

# bottom of the aquifer
botm = 0.0

# hydraulic conductivity
hk = 50.0

# boundary heads
h1 = 11.0

# build chd stress period data
chd_spd = {0: [[(0, 0, 0), h1]]}

strt = np.linspace(h1, h1, num=ncol)


# solver data
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

# well rate
well_q = [
    -45.846,
    -37.656,
    -32.229,
    -28.317,
    -25.339,
    -22.988,
    -21.082,
    -19.504,
    -18.176,
    -17.045,
]
well_spd = {}
for idx, q in enumerate(well_q):
    well_spd[idx] = [[(0, 0, ncol - 1), q]]

# ghb data
ghb_stage = -1.0
ghb_cond = 5.0
ghb_packname = "MYGHB"


def get_model(ws, name, api=False):
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option="all",
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
        newtonoptions="UNDER_RELAXATION",
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True, icelltype=1, k=hk)
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf, save_flows=True, iconvert=1, ss=0.0, sy=0.2, transient={0: True}
    )

    # chd file
    chd = flopy.mf6.ModflowGwfchd(gwf, stress_period_data=chd_spd)

    # api or well package
    if api:
        flopy.mf6.ModflowGwfapi(
            gwf,
            maxbound=1,
            pname=ghb_packname,
            print_flows=True,
            observations={"flow.obs.csv": [("Q", "API", (0, 0, ncol - 1))]},
        )
    else:
        flopy.mf6.ModflowGwfwel(
            gwf,
            stress_period_data=well_spd,
            pname=ghb_packname,
            observations={"flow.obs.csv": [("Q", "WEL", (0, 0, ncol - 1))]},
        )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )
    return sim


def build_model(idx, dir):
    # build MODFLOW 6 files
    ws = dir
    name = ex[idx]

    sim = get_model(ws, name)

    # build comparison model with zeroed values
    ws = os.path.join(dir, "libmf6")
    mc = get_model(ws, name, api=True)

    return sim, mc


def api_ghb_pak(hcof, rhs):
    hcof[0] = -ghb_cond
    rhs[0] = -ghb_cond * ghb_stage
    return hcof, rhs


def api_func(exe, idx, model_ws=None):
    success = False

    name = ex[idx].upper()
    if model_ws is None:
        model_ws = "."

    try:
        mf6 = ModflowApi(exe, working_directory=model_ws)
    except Exception as e:
        print("Failed to load " + exe)
        print("with message: " + str(e))
        return api_return(success, model_ws)

    # initialize the model
    try:
        mf6.initialize()
    except:
        return api_return(success, model_ws)

    # time loop
    current_time = mf6.get_current_time()
    end_time = mf6.get_end_time()

    # maximum outer iterations
    max_iter = mf6.get_value(mf6.get_var_address("MXITER", "SLN_1"))

    # get pointer to simulated heads
    head_tag = mf6.get_var_address("X", name.upper())
    head = mf6.get_value_ptr(head_tag)

    # get pointers to API data
    nbound_tag = mf6.get_var_address("NBOUND", name.upper(), ghb_packname)
    nbound = mf6.get_value_ptr(nbound_tag)
    nodelist_tag = mf6.get_var_address("NODELIST", name.upper(), ghb_packname)
    nodelist = mf6.get_value_ptr(nodelist_tag)
    hcof_tag = mf6.get_var_address("HCOF", name.upper(), ghb_packname)
    hcof = mf6.get_value_ptr(hcof_tag)
    rhs_tag = mf6.get_var_address("RHS", name.upper(), ghb_packname)
    rhs = mf6.get_value_ptr(rhs_tag)

    # set nbound and nodelist
    nbound[0] = 1
    nodelist[0] = ncol

    # model time loop
    while current_time < end_time:

        # get dt
        dt = mf6.get_time_step()

        # prepare... and reads the RIV data from file!
        mf6.prepare_time_step(dt)

        # convergence loop
        kiter = 0
        mf6.prepare_solve()

        while kiter < max_iter:
            # update api package
            hcof[:], rhs[:] = api_ghb_pak(
                hcof,
                rhs,
            )

            # solve with updated api data
            has_converged = mf6.solve()
            kiter += 1

            if has_converged:
                msg = f"Converged in {kiter}" + " outer iterations"
                print(msg)
                break

        # finalize time step
        mf6.finalize_solve()

        # finalize time step and update time
        mf6.finalize_time_step()
        current_time = mf6.get_current_time()

        # terminate if model did not converge
        if not has_converged:
            print("model did not converge")
            break

    # cleanup
    try:
        mf6.finalize()
        success = True
    except:
        return api_return(success, model_ws)

    # cleanup and return
    return api_return(success, model_ws)


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, idxsim=idx, api_func=api_func))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, idxsim=idx, api_func=api_func)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
