package mannyboidpaperrus.paperthinger.potioneffects;

import org.bukkit.entity.LivingEntity;
import org.bukkit.potion.PotionEffect;

/**
 * Handles vanilla potion effects.
 */
public class PotionEffectHandler {
    /**
     *  Tries to apply a potion effect if: 1, the entity doesn't already have the effect. B, the amplifier is stronger than the current effect. Or 3, the amplifier
     *  is the same, but the duration is longer than the current effect.
     *
     * @param e The entity to apply the effect to.
     * @param pe The effect to apply to the entity.
     * @return true, if successful.
     */
    public static boolean tryApplyPotionEffect(LivingEntity e, PotionEffect pe) {
        boolean successful = false;

        if (e.hasPotionEffect(pe.getType())) {
            PotionEffect current = e.getPotionEffect(pe.getType());

            if (current != null) {
                int amp = Math.abs(current.getAmplifier());

                if (amp < Math.abs(pe.getAmplifier())) {
                    successful = e.addPotionEffect(pe, true);

                } else if (amp == Math.abs(pe.getAmplifier())) {
                    int duration = current.getDuration();

                    if (duration < pe.getDuration()) {
                        successful = e.addPotionEffect(pe, true);
                    }
                }
            } else
                successful = e.addPotionEffect(pe, true);

        } else
            successful = e.addPotionEffect(pe, true);

        return successful;
    }
}
