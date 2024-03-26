package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import org.bukkit.attribute.Attribute;
import org.bukkit.attribute.AttributeInstance;
import org.bukkit.attribute.AttributeModifier;
import org.bukkit.entity.LivingEntity;

/**
 * Code to help manage entity attributes.
 */
public final class AttributeHelper {
    /**
     * Adds attribute modifiers to an attribute, skipping over those with the same name as a modifier that is already there.
     *
     * @param attribute The attribute to add the modifier to.
     * @param attributeModifiers The modifiers to add.
     */
    public static void addModifiersSafely(AttributeInstance attribute, AttributeModifier... attributeModifiers) {
        if (attribute != null)
            for (AttributeModifier attributeModifier : attributeModifiers) {
                String attributeModifierName = attributeModifier.getName();
                boolean alreadyHasModifier = false;

                for (AttributeModifier possibleDuplicate : attribute.getModifiers())
                    if (possibleDuplicate.getName().equals(attributeModifierName)) {
                        alreadyHasModifier = true;
                        break;
                    }

                if (!alreadyHasModifier)
                    attribute.addModifier(attributeModifier);
            }
    }

    /**
     * Removes all attribute modifiers that have the given name, or just one if removedOnce is true.
     *
     * @param attribute The attribute to remove the modifiers from.
     * @param modifierName The name of the modifiers to remove.
     * @param removeOnce Flag for whether or not to remove only one modifier.
     *
     * @return Whether or not the modifiers were removed.
     */
    public static boolean removeModifiers(AttributeInstance attribute, String modifierName, boolean removeOnce) {
        if (attribute != null) {
            boolean removedAModifier = false;

            for (AttributeModifier possibleDuplicate : attribute.getModifiers())
                if (possibleDuplicate.getName().equals(modifierName)) {
                    attribute.removeModifier(possibleDuplicate);
                    removedAModifier = true;

                    if (removeOnce)
                        break;
                }

            return removedAModifier;
        }

        return false;
    }

    /**
     * Removes all attribute modifiers that have the given name.
     *
     * @param attribute The attribute to remove the modifiers from.
     * @param modifierName The name of the modifiers to remove.
     *
     * @return Whether or not the modifiers were removed.
     */
    public static boolean removeModifiers(AttributeInstance attribute, String modifierName) {
        return removeModifiers(attribute, modifierName, false);
    }



    /**
     * Adds modifiers to a living entity's maxHealth attribute, if they have it, and scales their health accordingly.
     * Skips modifiers that have the same name as a modifier that is already there.
     *
     * @param livingEntity The living entity to add the modifier to.
     * @param maxHealthModifiers The modifiers to add to the living entity's max health.
     */
    public static void addHealthModifiersAndScale(LivingEntity livingEntity, AttributeModifier... maxHealthModifiers) {
        AttributeInstance maxHealth = livingEntity.getAttribute(Attribute.GENERIC_MAX_HEALTH);

        if (maxHealth != null) {
            double healthRatio = livingEntity.getHealth() / maxHealth.getValue();
            boolean addedModifiers = false;

            // Adds modifiers.
            for (AttributeModifier maxHealthModifier : maxHealthModifiers) {
                String attributeModifierName = maxHealthModifier.getName();
                boolean alreadyHasModifier = false;

                for (AttributeModifier possibleDuplicate : maxHealth.getModifiers())
                    if (possibleDuplicate.getName().equals(attributeModifierName)) {
                        alreadyHasModifier = true;
                        break;
                    }

                if (!alreadyHasModifier) {
                    maxHealth.addModifier(maxHealthModifier);
                    addedModifiers = true;
                }
            }

            // Scales health.
            if (addedModifiers)
                livingEntity.setHealth(maxHealth.getValue() * healthRatio);
        }
    }

    /**
     * Removes all max health modifiers of a given name that an entity has, or just one if removedOnce is true.
     * Scales the entity's afterward.
     *
     * @param livingEntity The living entity to remove attribute modifiers from.
     * @param modifierName The name of the modifiers to remove.
     * @param removeOnce Flag for whether or not to remove only one modifier.
     *
     * @return Whether or not the modifiers were removed.
     */
    public static boolean removeHealthModifiersAndScale(LivingEntity livingEntity, String modifierName, boolean removeOnce) {
        AttributeInstance maxHealth = livingEntity.getAttribute(Attribute.GENERIC_MAX_HEALTH);

        if (maxHealth != null) {
            boolean removedAModifier = false;

            for (AttributeModifier possibleDuplicate : maxHealth.getModifiers())
                if (possibleDuplicate.getName().equals(modifierName)) {
                    maxHealth.removeModifier(possibleDuplicate);
                    removedAModifier = true;

                    if (removeOnce)
                        break;
                }

            if (removedAModifier) {
                double healthRatio = livingEntity.getHealth() / maxHealth.getValue();
                livingEntity.setHealth(healthRatio * maxHealth.getValue());

                return true;
            }
        }

        return false;
    }

    /**
     * Removes all max health modifiers of a given name that an entity has.
     * Scales the entity's afterward.
     *
     * @param livingEntity The living entity to remove attribute modifiers from.
     * @param modifierName The name of the modifiers to remove.
     *
     * @return Whether or not the modifiers were removed.
     */
    public static boolean removeHealthModifiersAndScale(LivingEntity livingEntity, String modifierName) {
        return removeHealthModifiersAndScale(livingEntity, modifierName, false);
    }
}
