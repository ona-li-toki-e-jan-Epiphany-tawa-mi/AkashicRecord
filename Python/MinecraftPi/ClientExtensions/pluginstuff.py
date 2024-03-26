''' Used to access the methods stored in the main module. '''
main_module = {}


def _update(main_globals):
    ''' Gets the current state of the main module. '''
    global main_module
    main_module = main_globals


def get_instance():
    ''' Gets the current instance of minecraft, or none if there is not an instance. '''
    return main_module['_get_instance']()


def set_instance(instance):
    ''' Sets the current instance of minecraft. '''
    main_module['_set_instance'](instance)


def stop_program():
    ''' Stops the program. '''
    main_module['_stop_program']()