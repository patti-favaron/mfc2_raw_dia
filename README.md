# 'mfc2_raw_dia': a procedure for helping simple assessment of MeteoFlux&trade; Core V2 raw sonic files

Patrizia Favaron

## Introduction

'mfc2_raw_dia' is a command-line tool simplifying the checking of MeteoFlux&trade; Core V2 raw data against ultrasonic anemometer failures and temporary malfunctions.

Without 'mfc2_raw_dia' the task of checking the ultrasonic anemometer health would demand converting the binary raw MeteoFlux&trade; Core V2 files to readable form, and then analyzing them. This is not per-se difficult, but I admit to date few people are familiar with binary data, byte endianness, and similar low-level details: my procedure can then reveal as really useful.

In addition to checking data, the procedure also can convert MeteoFlux&trade; Core V2 raw data to SonicLib plain CSV form. This is not mandatory however, but the people who decide to follow this way will have another way for checking the ultrasonic anemometer operation, by comparing MeteoFlux&trade; results with eddy covariance processing made offline according to different assumptions.

## MeteoFlux&trade; Core V2, and its ultrasonic anemometer

At Servizi Territorio srl, as Italian distributors of Metek GmbH ultrasonic anemometers (and other sensors), decided to develop the MeteoFlux&trade; family of eddy covariance systems to simplify the use of the USA-1 three-dimensional ultrasonic anemometer.

The MeteoFlux&trade; evolved from version to version, since its inception in 1996 as a key product of EU LIFE/MoNiQA project. The version known as MeteoFlux&trade; Core V2 is a recent incarnation of the system, and constitutes the core of various eddy covariance stations operating in Italy and abroad.

To state things clearly, MeteoFlux&trade; Core V2 only supports the USA-1 and uSonic-3 models by Metek GmbH. No ultrasonic anemometer from other manufacturers is currently supported, nor we plan to support it in future.

## Can ultrasonic anemometers fail?

Thanks to their acoustic-only principle, ultrasonic anemometers are extremely robust sensors. Having no moving parts surely can be an advantage, and a big one in comparison to the electro-mechanical cup-vane anemometers whose bearings and electronics steadily deteriorate with time.

So it is tempting to imagine ultrasonic anemometers as "maintenance free" devices.

This imagination is, however, misleading: ultrasonic anemometers may malfunction, temporarily or definitively, due to improperly managed environmental conditions, failures due to mis-handling, burns by electrical discharges, and many other causes.

Among the failure conditions the following are prevailing:

- Icing of heads (the USA-1 and uSonic-3 exist in various configurations, and some of them have a heater whose purpose is precisely to prevent the formation of ice: this component however requires energy, and some clients prefer to not use it).
- Accumulation of dirt (being without moving parts makes things more reliable, but also permits an ideal support for spider nets, bird nests, and other; besides that, as time passes acoustic heads get covered of dust and slime).
- Alterations of geometry (during a maintenance intervention it may happen the sensor structure is hit and damaged).
- Electronic failures (caused for example by direct lightning, or improper use of analog lines).

Some of these failures result in temporary loss of data (for example icing, whose effect vanishes as soon as ice melts). Others have a probabilistic effect, with some data being produced and other invalidated, apparently at random. In some other cases (e.g. electronics failure) the ultrasonic anemometer simply ceases to release data.

## Error reporting on board of MteoFlux&trade; Core V2

Detecting the occurrence of these failures is simple, if the ultrasonic anemometer output is just dumped to a serial line without processing: the Metek protocol is pure text, and the sensor has a rich on-board plausibility checking and error reporting capability.

But if data are gathered and processed using an eddy covariance system then decoding must occur, and data are stored in compact binary form to save space. In MeteoFlux&trade; Core V2 errors are reported to binary files in a simple way by repeating the last "correct" line, and counting on the fact due to turbulent fluctuations the probability that two consecutive sonic quadruples are identical is practically zero, and can be safely neglected.

The program hosted in this repository, **mfc2_raw_dia**, is designed to interpreto the errors, count them and if required make them visible. This makes basic sensor diagnostics simpler, with the added bonus of converting MeteoFlux&trade; Core V2 binary files to SonicLib plain CSV format ([SonicLib](https://github.com/patti-favaron/SonicLib), contained in another repository, is an R collection of scripts for performing offline eddy covariance, developed by the Physics Department "Aldo Pontremoli" of the University of Milan).

As of the MeteoFlux&trade; Core V2, it is a system realized in-house and distributed by Servizi Territorio srl, and [you may find further information here](https://www.serviziterritorio.it).

## Procedure use

The procedure is invoked through the command line by the command

```
mfc2_raw_dia <Input_Directory> <Output_File> [<Output_Directory>]
```

where <Input_Directory> is the name of the directory containing the data, <Output_File> the name of a CSV file containing the results and <Output_Directory> the optional directory where the SonicLib files are saved.

It is worth noting the <Input_Directory> *must* conform to the Metek monthly data convention, that is, using Backus-Naur formalism,

```
<Input_Directory> ::= <Root_Directory>/YYYYMM
```

with YYYY an integer denoting the year, and MM the month (with leading zeros if necessary). For example:

```
/home/user/documents/meteoflux/data/202503
```

## A note on directory separator

The '/' directory separator is common to the UNIX and Linux world. In Windows the '\\' separating character is used instead.

When writing the procedure I had to choose which directory separator to adopt, and selected the UNIX version '/'.

The procedure can be compiled as is, and used in Linux/UNIX (the native operating system of MeteoFlux&trade; Core V2). But you are free to replace any occurrence of the character '/' with '\\\\' (two backslashes, not just one: some compilers used the '\\' character as an escape symbol, and two of the escape to a single one on compiling).

## Compiling sources

The repository is modeled after standard **fpm** projects (**fpm** stays for "Fortran package manager", and can be downloaded for both UNIX/Linux and Windows - it is a standard program, readily available on the net; just make sure it if the fpm you really need: there are various name overlaps - surfing the net for "Fortran package manager" is in my opinion the best way to reach).

To compile the source, just download the contents of this repository to a directory of your choice, enter it, then send the command

```
fpm build
```

Any modern Fortran compiler can be used. In case the compiler is not gfortran, then the build command becomes

```
fpm build --compiler <Your_Compiler_Name>
```

See anyway the **fpm** documentation.

## Output file contents

The output file is CSV-formatted, with a header line followed by as many data lines as the hours in the month specified by the YYYYMM subdirectory of <Input_Directory>.

The columns are:

- date: date and time (hour) to which the data line refers.
- total: total number of data lines in file.
- valid: number of valid lines in file.
- suspect: number of "suspect" data lines, that is the data lines which the sonic labelled as statistically not significant.
- invalid: number of data the anemometer considered outright invalid.
- vel: wind speed (m/s) according to the vector definition.
- vel.scalar: scalar wind speed (m/s).
- r: ratio between the vector and scalar velocity (dimensionless).
- slow.fraction: absolute fraction (between 0 and 1) of the number of data characterized by a wind speed less than 0.5 m/s, corresponding to a common activation threshold for professional-grade cup anemometers.
- dir: provenance direction (degrees from North, clockwise increasing).
- w: vertical wind speed (m/s).
- tke: turbulence kinetic energy (m2/s2).

## Things to look at first

The first thing to consider when evaluating the health of a sonic anemometer is checking how many valid data it produces per hour.

Ideally, sampling at 10Hz an ultrasonic anemometer releases 36000 quadruples per hour. The real number can differ, but is always very close to this value.

Also ideally, the fraction of valid to total data is 100%: you can check this by comparing the total and valid numbers.

In real situations, a small fraction of suspect and invalid data (say up to 10%) can be tolerated, and is usually indicative of dirty heads; a careful cleaning in this case is the most appropriate action.

Larger fractions of suspect and invalid data should be investigated with open mind.

Systematic data failures occurring principally on nighttime can indicate an icing problem.
