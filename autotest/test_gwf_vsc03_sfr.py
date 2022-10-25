# Scenario envisioned by this test is a river running through a V-shaped
# valley that loses water to the aquifer at the upper end until it goes
# dry, then begins to gain flow again at the lower reaches.  River water
# enters the simulation at 8 deg C.  Aquifer water starts out at 35 deg C.
# Reference viscosity temperature is 20 deg C.  With the VSC package active,
# the simulation should predict less loss of river water to the aquifer
# and more discharge of gw to the stream, compared to the same simulation
# with the VSC package inactive.

import sys
import math
from io import StringIO
import os
import shutil
import numpy as np
from subprocess import check_output
import flopy

# Append to system path to include the common subdirectory

sys.path.append(os.path.join("..", "common"))

# Import common functionality

import config
from figspecs import USGSFigure

mf6exe = os.path.abspath(config.mf6_exe)

# Setup scenario input
parameters = {
    "no-vsc-sfr01": {"viscosity_on": False},
    "vsc-sfr01": {"viscosity_on": True},
}

# Equation for determining land surface elevation with a stream running down the middle
def topElev_sfrCentered(x, y):
    return ((-0.003 * x) + 260.0) + (
        ((-2e-9 * (x - 5000.0)) + 1e-5) * (y + 1500.0) ** 2
    )


# Model units
length_units = "m"
time_units = "days"

# model domain and grid definition
Lx = 10000.0
Ly = 3000.0
nrow = 60
ncol = 200
nlay = 1
delr = Lx / ncol
delc = Ly / nrow
xmax = ncol * delr
ymax = nrow * delc
X, Y = np.meshgrid(
    np.linspace(delr / 2, xmax - delr / 2, ncol),
    np.linspace(ymax - delc / 2, 0 + delc / 2, nrow),
)
ibound = np.ones((nlay, nrow, ncol))
# Because eqn uses negative values in the Y direction, need to do a little manipulation
Y_m = -1 * np.flipud(Y)
top = topElev_sfrCentered(X, Y_m)
botm = np.zeros(top.shape)
strthd = top - 10.0

# NPF parameters
k11 = 1
ss = 0.00001
sy = 0.20
hani = 1
laytyp = 1

# Package boundary conditions
viscref = 8.904e-4

# time params
steady = {0: True, 1: False}
transient = {0: False, 1: True}
nstp = [1, 20]
tsmult = [1, 1]
perlen = [1, 20]

nouter, ninner = 1000, 300
hclose, rclose, relax = 1e-3, 1e-4, 0.97

# Transport related parameters
initial_temperature = 35.0  # Initial temperature (unitless)
porosity = 0.20  # porosity (unitless)
K_therm = 2.0  # Thermal conductivity  # ($W/m/C$)
rho_water = 1000  # Density of water ($kg/m^3$)
rho_solids = 2650  # Density of the aquifer material ($kg/m^3$)
C_p_w = 4180  # Heat Capacity of water ($J/kg/C$)
C_s = 880  # Heat capacity of the solids ($J/kg/C$)
D_m = K_therm / (porosity * rho_water * C_p_w)
rhob = (1 - porosity) * rho_solids  # Bulk density ($kg/m^3$)
K_d = C_s / (rho_water * C_p_w)  # Partitioning coefficient ($m^3/kg$)


# MODFLOW 6 flopy GWF & GWT simulation object (sim) is returned
#
def build_model(key, viscosity_on=False):
    print("Building model...{}".format(key))

    # Base simulation and model name and workspace
    ws = os.path.join("temp", "examples", key)

    # generate names for each model
    name = "vsc03"
    gwfname = "gwf-" + key
    gwtname = "gwt-" + key

    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name=mf6exe)

    tdis_rc = []
    for i in range(len(nstp)):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    flopy.mf6.ModflowTdis(
        sim, nper=len(nstp), perioddata=tdis_rc, time_units=time_units
    )

    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=gwfname, save_flows=True, newtonoptions="newton"
    )

    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="cooley",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename="{}.ims".format(gwfname),
    )
    sim.register_ims_package(ims, [gwfname])

    # Instantiate discretization package
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
    )

    # Instantiate node property flow package
    flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        icelltype=1,  # >0 means saturated thickness varies with computed head
        k=k11,
    )

    # Instantiate storage package
    flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=ss,
        sy=sy,
        steady_state=steady,
        transient=transient,
    )

    # Instantiate initial conditions package
    flopy.mf6.ModflowGwfic(gwf, strt=strthd)

    # Instantiate viscosity package
    if viscosity_on:
        vsc_filerecord = "{}.vsc.bin".format(gwfname)
        vsc_pd = [(0, 0.0, 20.0, gwtname, "TEMPERATURE")]
        flopy.mf6.ModflowGwfvsc(
            gwf,
            viscref=viscref,
            viscosity_filerecord=vsc_filerecord,
            viscosityfuncrecord=[("nonlinear", 10.0, 248.37, 133.15)],
            nviscspecies=len(vsc_pd),
            packagedata=vsc_pd,
            pname="vsc",
            filename="{}.vsc".format(gwfname),
        )

    # Instantiate output control package
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "LAST")],
    )

    # Instantiate recharge package
    # total inflow 2000.0 on each side (4,000 total)
    rech = np.zeros_like(top)
    rech_rate_lo = 0.001
    rech_rate_hi = 0.015
    for i in np.arange(ncol):
        rech[0, i] = rech_rate_lo + (rech_rate_hi - rech_rate_lo) / ncol * i

    rech[-1, :] = rech[0, :]
    irch = np.zeros_like(rech)
    irch = irch.astype(int)
    temperature_array = np.ones_like(irch) * 15.0
    aux = {0: [temperature_array]}
    flopy.mf6.ModflowGwfrcha(
        gwf,
        print_flows=True,
        recharge=rech,
        irch=irch,
        auxiliary=["TEMPERATURE"],
        aux=aux,
        pname="RCHA-1",
        filename="{}.rcha".format(gwfname),
    )

    # Instantiate evapotranspiration package
    # ET rate is 0.003 everywhere in the model
    evtr_lo = 0.0001
    evtr_hi = 0.012
    extdp_hi = 30
    extdp_lo = 10
    evtspd = []
    for i in np.arange(nrow):
        for j in np.arange(ncol):
            evtr = evtr_hi - (evtr_hi - evtr_lo) / ncol * j
            extdp = extdp_hi - (extdp_hi - extdp_lo) / ncol * j
            #                 cellid,   surface, rate, depth, [pxdp], [petm], [petm0], [aux]
            evtspd.append([(0, i, j), top[i, j], evtr, extdp, 1.0, 0.0])
    surf_rate_specified = True
    flopy.mf6.ModflowGwfevt(
        gwf,
        print_flows=False,
        surf_rate_specified=surf_rate_specified,
        maxbound=nrow * ncol,
        nseg=1,
        stress_period_data=evtspd,
        auxiliary="TEMPERATURE",
        pname="EVT-1",
        filename="{}.evt".format(gwfname),
    )

    # Instantiate streamflow routing package

    # Determine the middle row and store in rMid (account for 0-base)
    rMid = nrow // 2 - 1
    # sfr data
    nreaches = ncol
    rlen = delr
    rwid = 7.0
    roughness = 0.035
    rbth = 1.0
    rhk = 1.0
    strm_up = 254.899750
    strm_dn = 225.150250
    slope = (strm_up - strm_dn) / ((ncol - 1) * delr)
    ustrf = 1.0
    ndv = 0
    strm_incision = 10
    viscaux = 1.111111111
    temperatureaux = 8.0

    packagedata = []
    for irch in range(nreaches):
        nconn = 1
        if 0 < irch < nreaches - 1:
            nconn += 1
        rp = [
            irch,
            (0, rMid, irch),
            rlen,
            rwid,
            slope,
            top[rMid, irch] - strm_incision,
            rbth,
            rhk,
            roughness,
            nconn,
            ustrf,
            ndv,
            viscaux,
            temperatureaux,
        ]
        packagedata.append(rp)

    connectiondata = []
    for irch in range(nreaches):
        rc = [irch]
        if irch > 0:
            rc.append(irch - 1)
        if irch < nreaches - 1:
            rc.append(-(irch + 1))
        connectiondata.append(rc)

    inflow_loc = 0
    sfr_perioddata = [
        [inflow_loc, "inflow", 25000.0],
    ]
    sfr_perioddata = {0: sfr_perioddata}

    budpth = f"{gwfname}.sfr.cbc"
    flopy.mf6.ModflowGwfsfr(
        gwf,
        save_flows=True,
        print_stage=True,
        print_flows=True,
        print_input=False,
        auxiliary=["VDUMMY", "TEMPERATURE"],
        unit_conversion=1.486 * 86400,
        budget_filerecord=budpth,
        mover=False,
        nreaches=nreaches,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=sfr_perioddata,
        pname="SFR-1",
        filename="{}.sfr".format(gwfname),
    )

    # Setup the GWT model for simulating heat transport
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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
        filename="{}.ims".format(gwtname),
    )
    sim.register_ims_package(imsgwt, [gwtname])
    flopy.mf6.ModflowGwtdis(
        gwt,
        length_units=length_units,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # Instantiate Mobile Storage and Transfer package
    flopy.mf6.ModflowGwtmst(
        gwt,
        porosity=porosity,
        sorption="linear",
        bulk_density=rhob,
        distcoef=K_d,
        pname="MST-1",
        filename="{}.mst".format(gwtname),
    )

    # Instantiate Transport Initial Conditions package
    flopy.mf6.ModflowGwtic(gwt, strt=initial_temperature)

    # Instantiate Advection package
    flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")

    # Instantiate Dispersion package (also handles conduction)
    flopy.mf6.ModflowGwtdsp(gwt, xt3d_off=True, diffc=D_m)

    # Instantiate Source/Sink Mixing package
    sourcerecarray = [
        ("RCHA-1", "AUX", "TEMPERATURE"),
        ("EVT-1", "AUX", "TEMPERATURE"),
    ]
    flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

    # Instantiate Streamflow Transport package
    sftpackagedata = []
    for irno in range(ncol):
        t = (irno, 0.0)
        sftpackagedata.append(t)

    sftperioddata = [(0, "STATUS", "CONSTANT"), (0, "CONCENTRATION", 8.0)]

    flopy.mf6.modflow.ModflowGwtsft(
        gwt,
        boundnames=False,
        save_flows=True,
        print_input=False,
        print_flows=True,
        print_concentration=True,
        concentration_filerecord=gwtname + ".sft.bin",
        budget_filerecord=gwtname + ".sft.bud",
        packagedata=sftpackagedata,
        reachperioddata=sftperioddata,
        flow_package_auxiliary_name="TEMPERATURE",
        flow_package_name="SFR-1",
        pname="SFT-1",
        filename="{}.sft".format(gwtname),
    )

    # Instantiate Output Control package for transport
    flopy.mf6.ModflowGwtoc(
        gwt,
        concentration_filerecord="{}.ucn".format(gwtname),
        saverecord=[("CONCENTRATION", "ALL")],
        printrecord=[("CONCENTRATION", "LAST"), ("BUDGET", "LAST")],
        filename="{}.oc".format(gwtname),
    )

    # Instantiate Gwf-Gwt Exchange package
    flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename="{}.gwfgwt".format(gwtname),
    )

    return sim


# Function to write model files
def write_and_run_model(sim, key, silent=True):
    sim.write_simulation(silent=silent)

    success, buff = sim.run_simulation(silent=False)
    errmsg = f"simulation did not terminate successfully\n{buff}"
    assert success, errmsg

    # slurp in sfr cell-by-cell budgets
    simpath = sim.simulation_data.mfpath.get_sim_path()

    sfrbud = "gwf-" + key + ".sfr.cbc"
    fpth = os.path.join(simpath, sfrbud)
    budobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    bud = budobj.get_data(text="             GWF")

    return bud[-1]


def scenario(idx, silent=True):
    # Two model runs to check relative flows w/ and w/o VSC

    # Model Run 1 (Do not account for the effects of viscosity)
    # Model Run 2 (Account for the effects of viscosity)
    # ---------------------------------------------------------
    key = list(parameters.keys())[idx]
    parameter_dict = parameters[key]
    sim = build_model(key, **parameter_dict)
    sfr_rbr_bud = write_and_run_model(sim, key, silent=silent)

    return sfr_rbr_bud


def confirm_run_results(no_vsc_bud, with_vsc_bud):
    # sum up total losses and total gains in the first 10 reaches
    # and the last 10 reaches
    for i in np.arange(10):
        # upper reaches
        assert abs(no_vsc_bud[i][2]) > abs(
            with_vsc_bud[i][2]
        ), "GW/SW not as expected"

        # lower reaches
        assert abs(no_vsc_bud[-(i + 1)][2]) < abs(
            with_vsc_bud[-(i + 1)][2]
        ), "GW/SW not as expected"


def test_01():
    # Compare model runs with and without viscosity package active
    # Model 1 - no viscosity
    sfr_scen1_bud = scenario(0, silent=False)

    # Model 2 - include viscosity
    sfr_scen2_bud = scenario(1, silent=False)

    # Seepage from upper SFR reaches should decrease owing to cold
    # stream water.  GW discharge back to lower SFR reaches should
    # increase owing to hot groundwater
    confirm_changes(sfr_scen1_bud, sfr_scen2_bud)


# nosetest end

if __name__ == "__main__":
    # ### V-shaped, linear river valley model

    # Compare model runs with and without viscosity package active
    # Model 1 - no viscosity
    sfr_scen1_bud = scenario(0)

    # Model 2 - include viscosity
    sfr_scen2_bud = scenario(1)

    # Seepage from upper SFR reaches should decrease owing to cold
    # stream water.  GW discharge back to lower SFR reaches should
    # increase owing to hot groundwater
    confirm_run_results(sfr_scen1_bud, sfr_scen2_bud)
