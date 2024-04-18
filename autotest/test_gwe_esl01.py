"""
A "bathtub" test problem for GWE - bathtub meaning a single cell model
(and actually, the bathtub is dry in this example)

Test the energy source loading package by warming a single cell with a known
amount of energy input.

 Model configuration
 
    ~: Represents energy source loading

       +---------+
       |         |
       |    ~    |
       |         |
       +---------+

"""

# Imports

import os
import numpy as np
import pytest
import flopy

from autotest.framework import TestFramework


# Base simulation and model name and workspace

scheme = "UPSTREAM"
# scheme = "TVD"

cases = [
    "warmup",  # 1-cell "bathtub" model with easily calculate-able answers
]

# Model units
length_units = "meters"
time_units = "days"

# Parameterization

nrow = ncol = nlay = 1
top = 1.0
botm = [0.0]
delr = 1.0  # Column width ($m$)
delc = 1.0  # Row width ($m$)
k11 = 1.0  # Horizontal hydraulic conductivity ($m/d$)
ss = 1e-6  # Specific storage
sy = 0.10  # Specific Yield
prsity = sy  # Porosity
nper = 4  # Number of periods
perlen = [1, 1, 1, 1]  # Simulation time ($days$)
nstp = [1, 1, 1, 1]  # 10 day transient time steps
steady = {0: False}
transient = {0: True}
strthd = 0.0

# Set some static model parameter values

k33 = k11  # Vertical hydraulic conductivity ($m/d$)
idomain = 1  # All cells included in the simulation
iconvert = 1  # All cells are convertible

icelltype = 1  # Cell conversion type (>1: unconfined)

# Set some static transport related model parameter values
strt_temp1 = 0.0
dispersivity = 0.0  # dispersion (remember, 1D model)

# GWE related parameters
rhow = 1000.0
cpw = 4183.0
lhv = 2454.0
cps = 760.0
rhos = 1500.0

# Set solver parameter values (and related)
nouter, ninner = 100, 300
hclose, rclose, relax = 1e-10, 1e-10, 1.0
ttsmult = 1.0

# Set up temporal data used by TDIS file
tdis_rc = []
for i in np.arange(nper):
    tdis_rc.append((perlen[i], nstp[i], ttsmult))

Joules_added_for_1degC_rise = (
    delr * delc * (top - botm[0]) * (1 - prsity) * cps * rhos
)

# ### Create MODFLOW 6 GWE
#
# No GWF, only Heat conduction simulated


def build_models(idx, test):
    # Base MF6 GWF model type
    ws = test.workspace
    name = cases[idx]

    print("Building MF6 model...()".format(name))

    # generate names for each model
    gwfname = "gwf-" + name
    gwename = "gwe-" + name

    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=ws, exe_name="mf6", version="mf6"
    )

    # Instantiating MODFLOW 6 time discretization
    flopy.mf6.ModflowTdis(
        sim, nper=nper, perioddata=tdis_rc, time_units=time_units
    )

    # Instantiating MODFLOW 6 groundwater flow model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        save_flows=True,
        model_nam_file="{}.nam".format(gwfname),
    )

    # Instantiating MODFLOW 6 solver for flow model
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(imsgwf, [gwfname])

    # Instantiating MODFLOW 6 discretization package
    flopy.mf6.ModflowGwfdis(
        gwf,
        length_units=length_units,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1,
        pname="DIS-1",
        filename="{}.dis".format(gwfname),
    )

    # Instantiating MODFLOW 6 storage package
    flopy.mf6.ModflowGwfsto(
        gwf,
        ss=ss,
        sy=sy,
        iconvert=iconvert,
        steady_state=steady,
        transient=transient,
        pname="STO",
        filename="{}.sto".format(gwfname),
    )

    # Instantiating MODFLOW 6 node-property flow package
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=True,
        icelltype=icelltype,
        k=k11,
        k33=k33,
        save_specific_discharge=True,
        pname="NPF-1",
        filename="{}.npf".format(gwfname),
    )

    # Instantiating MODFLOW 6 initial conditions package for flow model
    flopy.mf6.ModflowGwfic(
        gwf,
        strt=strthd,
        pname="IC-HD",
        filename="{}.ic".format(gwfname),
    )

    # Instantiating MODFLOW 6 output control package for flow model
    flopy.mf6.ModflowGwfoc(
        gwf,
        pname="OC-1",
        head_filerecord="{}.hds".format(gwfname),
        budget_filerecord="{}.cbc".format(gwfname),
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # ----------------------------------
    # Instantiating MODFLOW 6 GWE model
    # ----------------------------------
    gwe = flopy.mf6.ModflowGwe(
        sim, modelname=gwename, model_nam_file="{}.nam".format(gwename)
    )
    gwe.name_file.save_flows = True

    imsgwe = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwename),
    )
    sim.register_ims_package(imsgwe, [gwe.name])

    # Instantiating MODFLOW 6 transport discretization package
    flopy.mf6.ModflowGwedis(
        gwe,
        nogrb=True,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1,
        pname="DIS-GWE",
        filename="{}.dis".format(gwename),
    )

    # Instantiating MODFLOW 6 transport initial concentrations
    flopy.mf6.ModflowGweic(
        gwe, strt=strt_temp1, pname="IC-2", filename="{}.ic".format(gwename)
    )

    # Instantiating MODFLOW 6 transport advection package
    flopy.mf6.ModflowGweadv(
        gwe, scheme=scheme, pname="ADV-2", filename="{}.adv".format(gwename)
    )

    # Instantiating MODFLOW 6 transport dispersion package
    flopy.mf6.ModflowGwecnd(
        gwe,
        xt3d_off=True,
        alh=dispersivity,
        ath1=dispersivity,
        ktw=0.5918 * 86400,
        kts=0.2700 * 86400,
        pname="CND-2",
        filename="{}.cnd".format(gwename),
    )

    # Instantiating MODFLOW 6 transport mass storage package (formerly "reaction" package in MT3DMS)
    flopy.mf6.ModflowGweest(
        gwe,
        save_flows=True,
        porosity=prsity,
        cps=cps,
        rhos=rhos,
        packagedata=[cpw, rhow, lhv],
        pname="EST-2",
        filename="{}.est".format(gwename),
    )

    # Instantiate MODFLOW 6 heat transport output control package
    flopy.mf6.ModflowGweoc(
        gwe,
        pname="OC-1",
        budget_filerecord="{}.cbc".format(gwename),
        temperature_filerecord="{}.ucn".format(gwename),
        temperatureprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("TEMPERATURE", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("TEMPERATURE", "ALL"), ("BUDGET", "ALL")],
    )

    # Instantiate energy source loading (ESL) package
    # Energy is added such that the temperature change in the cell will be
    # +1.0, +2.0, -1.0, and 0.0 degrees Celcius from stress period to stress
    # period
    esl_spd = {
        0: [
            [(0, 0, 0), Joules_added_for_1degC_rise],
        ],
        1: [[(0, 0, 0), 2 * Joules_added_for_1degC_rise]],
        2: [[(0, 0, 0), -1 * Joules_added_for_1degC_rise]],
        3: [],
    }
    flopy.mf6.ModflowGweesl(
        gwe,
        stress_period_data=esl_spd,
        pname="ESL-1",
        filename="{}.esl".format(gwename),
    )

    # Instantiating MODFLOW 6 flow-transport exchange mechanism
    flopy.mf6.ModflowGwfgwe(
        sim,
        exgtype="GWF6-GWE6",
        exgmnamea=gwfname,
        exgmnameb=gwename,
        pname="GWFGWE1",
        filename="{}.gwfgwe1".format(gwename),
    )

    return sim, None


def check_output(idx, test):
    print("evaluating results...")

    # read transport results from GWE model
    name = cases[idx]
    gwename = "gwe-" + name

    fpth = os.path.join(test.workspace, f"{gwename}.ucn")

    try:
        # load temperatures
        tobj = flopy.utils.HeadFile(
            fpth, precision="double", text="TEMPERATURE"
        )
        temps = tobj.get_alldata()
    except:
        assert False, f'could not load temperature data from "{fpth}"'

    # Energy source loading was carefully chosen to result in a +1,
    # +2 (for an absolute value of 3 deg C), -1, and 0.0 degree C
    # change between stress periods.
    known_ans = [1.0, 3.0, 2.0, 2.0]
    msg0 = (
        "Grid cell temperatures do not reflect the expected difference "
        "in stress period "
    )
    for index in np.arange(4):
        assert np.isclose(temps[index, 0, 0, 0], known_ans[index]), msg0 + str(
            index
        )


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(cases)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
