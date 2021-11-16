!> \page code_structure MAM Code Structure
!!
!! MAM code is being developed to demonstrate the possibility of
!! applying a common interface for aerosol packages that host models can
!! use to maintan the state of an aerosol system, retrieve aerosol
!! properties required by other model components, and advance the aerosol
!! state during a simulation.
!!
!! It is also being developed to apply contemporary best practice for
!! design and testing. This document lays out the science requirements and
!! design principles for MAM, provides an overview of the refactored
!! codebase, and describes in more detail specific parts of the code where
!! object-oriented design patterns have been applied to components of the
!! MAM codebase.
!!
!! ## Software Requirements ##
!!
!! ### Aerosol&ndash;Radiation Interface ###
!!
!! - Permit radiation schemes to use whatever wavelength grid they want
!!   and not have to be aware of the native wavelength grid of the
!!   aerosol package
!!
!! - Perform expensive setup for optical property calculations during
!!   initialization (reading files, setting up interpolations, etc.)
!!
!! - Remove assumptions of the way aerosols are represented from the
!!   radiation code (mode number, size, shape, sectional schemes, etc.)
!!
!! - Allow radiation schemes interested in optical properties to maintain
!!   their own wavelength grids (i.e., not have aerosol packages maintain
!!   grids for every scheme that needs aerosol optical properties)
!!
!! ## Design Principles ##
!!
!! MAM is being developed according to the
!! [recommendations for MUSICA contributors](https://ncar.github.io/musica-core/html/contributors.html)
!! and following the
!! [MUSICA style guide](https://ncar.github.io/musica-core/html/coding_style.html).
!! Here we outline some specific design principles from these guides
!! that are particularly relevant to the MAM code structure, and briefly
!! describe how they are acheived.
!!
!! - An aerosol package shoud compile as a library with a clearly defined
!!   application programming interface (API).
!!   - The [MAM GitHub repository](https://github.com/NCAR/mam)
!!     includes CMake configurations that
!!     allow a user to build the MAM library. Interactions with MAM primarily
!!     occur through instances of the \c mam_core::core_t class. Documentation for the
!!     \c mam_core::core_t class acts as the MAM API. Integration tests included
!!     in the repository can be used as examples of how to use the MAM API.
!!
!! - An aerosol package should be comprised of functions that are of
!!   limited, clearly defined scope and purpose for readability and testability.
!!   - Functions are almost all less than 40 lines long. Functions are
!!     designed to do one specific task, with clear documentation. Function
!!     arguments are clearly defined with units.
!!
!! - An aerosol package should employ automated testing with a
!!   combination of integration and unit tests, with a goal of 100% code
!!   coverage by unit tests.
!!   - The MAM library, along with the \c aerosol and \c musica-core
!!     libraries it links to, use CMake with CTest to maintain the suite of
!!     tests. GitHub Actions automates testing, document generation, and code
!!     coverage assessment using \c gcov. [CodeCov.io](https://about.codecov.io/)
!!     is used to visualize the coverage reports.
!!
!! - An aerosol package should employ data and implementation hiding to
!!   the extent possible to facilitate the interoperability of aerosol
!!   packages with distinct parameterizations and ways of representing
!!   aerosol systems.
!!   - The MAM code base is object-oriented with almost entirely private
!!     class data members. Aerosol information and functionality are exposed
!!     through type-bound procedures of each class.
!!
!! ## Codebase Overview ##
!!
!! ### Library Structure ###
!!
!! The MAM software packages comprises three libraries, each of which
!! provides specific functionality required to simulate the modal aerosol
!! system.
!!
!! - \b musica-core: The [musica-core](https://github.com/NCAR/musica-core)
!!   library includes a set of classes that perform general, reusable
!!   tasks and are applicable to a variety of MUSICA software components,
!!   including MAM. Some \c musica-core classes used by MAM are
!!   \c string_t, \c config_t, \c io_t, \c grid_t, and \c
!!   interpolator_t. These are described in more detail below and in the
!!   [musica-core documentation](https://ncar.github.io/musica-core/index.html)
!!
!! - \b aerosol-interface: The
!!   [aerosol-interface](https://github.com/NCAR/aerosol-interface)
!!   library defines the common aerosol API used for MUSICA aerosol
!!   packages including MAM.
!!   The \c aerosol-interface library also includes some classes for
!!   general use by aerosol packages that expose the API.
!!   These include the \c wavelength_grid_t, optics_t, and
!!   \c environmental_state_t classes.
!!
!! - \b mam: The [mam](https://github.com/NCAR/mam) library provides
!!   the core functionality needed to simulate the modal aerosol system
!!   exposed through the common aerosol API. The primary interactions
!!   through instances of the \c mam_core::core_t class, which manages
!!   MAM aerosol configuration, and instances of the
!!   \c mam_core::state_t class, which maintains the state of a MAM
!!   aerosol.
!!
!! ### Object Structure ###
!!
!! The MAM class diagram including classes from the \c aerosol-interface
!! and \c musica-core libraries is shown below
!!
!! \htmlinclude doc/doxygen_files/images/mam-class-diagram.html
!!
!! The boxes of various color indicate general functionality provided by
!! sets of related classes.
!!
!! Things to note in the class structure:
!!
!! - both the whole MAM aerosol and individual modes extend the aerosol
!!   API. This would allow a radiation or other package work with the
!!   full set of modes or a single mode interchangeably, similar to the
!!   effect of applying the Composite Pattern.
!!
!! - Interpolators apply the Strategy Pattern, whereby you can specify
!!   how you want an interpolation to be calculated when you create an
!!   \b interpolator_t object. See
!!   [here](https://ncar.github.io/musica-core/html/structmusica__interpolator_1_1interpolator__t.html#details)
!!   for an example of how to use \c interpolator_t objects.
!!
!! - Reusable components, like \c interpolator_t, \c lookup_axis_t,
!!   \c lookup_2D_axis_t, and \c grid_t, are
!!   included in the \c musica-core library and can be extended if needed
!!   for specific applications, as in the case of \c wavelength_grid_t.
!!
!! - Class data members are private.
!!
!! - Class, variable, and function names are meaningful and include
!!   units where applicable
!!
!! ### Workflow ###
!!
!! The over workflow for MAM interactions with radiaion are show below.
!!
!! #### Initialization ####
!! \htmlinclude doc/doxygen_files/images/mam-radiation-workflow-initialization.html
!!
!! #### Run-Time ####
!! \htmlinclude doc/doxygen_files/images/mam-radiation-workflow-runtime.html
!!
!! ### Testing ###
!!
!! Tests are stored in the \c test/ folder with integration tests in
!! \c test/integration/ and unit tests in \c test/unit/. There are
!! currently two integration tests:
!!
!! - \b current_cam: This test was extracted from the CAM code base and
!! used to test against during development of the refactored MAM--RRTMG
!! interactions.
!!
!! - \b radiation: This test serves as an example of how an arbitrary
!! radiation package can interact with MAM. The results of the optical
!! property calculations are not evaluated.
!!
!! The unit tests are organized into files of the same name as those in
!! \b src/ with additional mock classes and configuration files.
!!
