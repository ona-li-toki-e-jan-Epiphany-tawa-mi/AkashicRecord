package lllllllllllllllll.abhorrentideas.badideas;

import net.minecraft.util.DamageSource;
import net.minecraftforge.event.entity.living.LivingHurtEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

/**
 * Spiky things instantly kill you.
 *
 * Idea by: u/DoubleAgentBlumaroo
 * On: r/shittymcsuggestions
 */
public class OneHitSpike {
    @SubscribeEvent
    public void onHurtBySpike(LivingHurtEvent livingHurtEvent) {
        if (!livingHurtEvent.isCanceled()) {
            DamageSource damageSource = livingHurtEvent.getSource();
            boolean validDamageSource = false;

            if (damageSource.equals(DamageSource.CACTUS)) {
                validDamageSource = true;

            } else
                switch (damageSource.damageType) {
                    case "thorns":
                        validDamageSource = true;
                }

            if (validDamageSource) {
                livingHurtEvent.setAmount(Float.MAX_VALUE);
            }
        }
    }
}
