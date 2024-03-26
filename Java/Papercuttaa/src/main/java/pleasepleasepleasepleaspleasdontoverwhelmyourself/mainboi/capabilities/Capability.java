package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.capabilities;

import org.bukkit.entity.Entity;

/**
 * The base class for capabilities.
 * Extend this class to get the necessary functions for capabilities.
 */
public abstract class Capability {
    /**
     * The constructor. Although it must contain the same parameters, nothing needs to be done with them.
     *
     * Extra data is a way to save data to each individual entities' instance of a capability.
     * It is passed to the constructor in the form of a string, and it is up to the capability to decide how to use it.
     *
     * @param extraData Extra data that has been placed on a tag.
     */
    public Capability(String extraData) {}

    /**
     * A function to access the constructor of a capability through its instances.
     * Function must be properly implemented for the capability to work.
     *
     * Example:
     *  @Override
     *  public SomeCapability useConstructor(String extraData) {
     *      return new SomeCapability(extraData);
     *  }
     *
     * @param extraData The extra data to be supplied to the constructor.
     *
     * @return The created instance of the capability.
     */
    public abstract Capability useConstructor(String extraData);

    /**
     * The name of the capability.
     * Used to store capabilities in the form of tags on entities.
     *
     * @return The name of the capability.
     */
    public abstract String getCapabilityName();



    /**
     * The extra data of the capability.
     * Used to store and relay the extra data of capabilities.
     *
     * Override to gain extra data functionality.
     *
     * @return The extra data a capability has.
     */
    public String getExtraData() {
        return "";
    }

    /**
     * Returns true if the capability is lost on death (only applies to players.)
     *
     * Override to set volatility.
     *
     * @return Whether or not the capability is volatile.
     */
    public boolean isVolatile() {
        return false;
    }



    /**
     * Runs a capability, allowing it to apply its effects.
     *
     * @param entity The entity to apply the capability's effects to.
     */
    public void runCapability(Entity entity) {}

    /**
     * Runs a capability upon its assignment to an entity.
     *
     * @param entity The entity the capability was assigned to.
     */
    public void onAssignment(Entity entity) {}

    /**
     * Runs a capability upon it being revoked from an entity.
     *
     * @param entity The entity the capability was revoked from.
     */
    public void onRevoke(Entity entity) {}
}
