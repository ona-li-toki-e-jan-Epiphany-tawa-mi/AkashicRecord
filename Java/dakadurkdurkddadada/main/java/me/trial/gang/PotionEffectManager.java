package me.trial.gang;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.Particle;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.potion.PotionEffect;
import org.bukkit.scheduler.BukkitRunnable;

class PotionEffectManager {
    static void tryApplyPotionEffect(Player p, PotionEffect pe, boolean force) {
        if (p.hasPotionEffect(pe.getType())) {
            PotionEffect current = p.getPotionEffect(pe.getType());
            int amp = Math.abs(current.getAmplifier());

            if (amp < Math.abs(pe.getAmplifier()))
                p.addPotionEffect(pe, force);
            else if (amp == Math.abs(pe.getAmplifier())) {
                int duration = current.getDuration();

                if (duration < pe.getDuration()) {
                    p.addPotionEffect(pe, force);
                }
            }
        } else
            p.addPotionEffect(pe, force);
    }
    static void tryApplyPotionEffect(LivingEntity e, PotionEffect pe, boolean force) {
        if (e.hasPotionEffect(pe.getType())) {
            PotionEffect current = e.getPotionEffect(pe.getType());
            int amp = Math.abs(current.getAmplifier());

            if (amp < Math.abs(pe.getAmplifier()))
                e.addPotionEffect(pe, force);
            else if (amp == Math.abs(pe.getAmplifier())) {
                int duration = current.getDuration();

                if (duration < pe.getDuration()) {
                    e.addPotionEffect(pe, force);
                }
            }
        } else
            e.addPotionEffect(pe, force);
    }

    static void addCustomPotionEffect(Player p, CustomPotionEffect cPotionEffect, int amplifer, int duration) {
        if (duration < 0)
            return;
        amplifer %= 256;

        switch (cPotionEffect) {
            case BLEEDING:
                Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedTime").getScore(p.getName()).setScore(duration);
                Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedAmp").getScore(p.getName()).setScore(amplifer);
                p.addScoreboardTag("bleeding");
                long bledInter = 256 - Math.abs(amplifer);

                int finalAmplifer = amplifer;
                ItemStack bleedColor = new ItemStack(Material.REDSTONE);
                new BukkitRunnable() {
                    @Override
                    public void run() {
                        if (!p.getScoreboardTags().contains("bleeding"))
                            cancel();
                        if (p.isDead()) {
                            p.removeScoreboardTag("bleeding");
                            cancel();
                        }

                        if (finalAmplifer < 0)
                            Gang.heal(p, 1);
                        else
                            p.damage(1);

                        p.getWorld().spawnParticle(Particle.ITEM_CRACK, p.getLocation().add(0, p.getHeight() / 4, 0), Math.abs(finalAmplifer), 0.1, 0.1, 0.1, 0.1, bleedColor, true);
                    }
                }.runTaskTimer(Gang.getInstance(), 0, bledInter);

                break;
        }
    }
    static void addCustomPotionEffect(LivingEntity e, CustomPotionEffect cPotionEffect, int amplifer, int duration) {
        if (duration < 0)
            return;
        amplifer %= 256;

        switch (cPotionEffect) {
            case BLEEDING:
                if (e.getType() == EntityType.SKELETON || e.getType() == EntityType.SKELETON_HORSE || e.getType() == EntityType.WITHER_SKELETON ||
                        e.getType() == EntityType.WITHER || e.getType() == EntityType.STRAY)
                    return;

                Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedTime").getScore(e.getName()).setScore(amplifer);
                Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedAmp").getScore(e.getName()).setScore(amplifer);
                e.addScoreboardTag("bleeding");
                long bledInter = 256 - Math.abs(amplifer);

                int finalAmplifer = amplifer;
                ItemStack bleedColor = new ItemStack(Material.REDSTONE);
                new BukkitRunnable() {
                    @Override
                    public void run() {
                        if (!e.getScoreboardTags().contains("bleeding"))
                            cancel();
                        if (e.isDead()) {
                            e.removeScoreboardTag("bleeding");
                            cancel();
                        }

                        if (finalAmplifer < 0) {
                            Gang.heal(e, 1);
                        } else
                            e.damage(1);

                        e.getWorld().spawnParticle(Particle.ITEM_CRACK, e.getLocation().add(0, e.getHeight() / 4, 0), Math.abs(finalAmplifer), 0.1, 0.1, 0.1, 0.1, bleedColor, true);
                    }
                }.runTaskTimer(Gang.getInstance(), 0, bledInter);
                break;
        }
    }

    static void tryApplyCustomPotionEffect(Player p, CustomPotionEffect cPotionEffect, int amplifer, int duration) {
        if (duration < 0)
            return;
        amplifer %= 256;
        int ampVal, durato;

        switch (cPotionEffect) {
            case BLEEDING:
                if (!p.getScoreboardTags().contains("bleeding")) {
                    addCustomPotionEffect(p, cPotionEffect, amplifer, duration);
                    return;
                }
                ampVal = Math.abs(Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedTime").getScore(p.getName()).getScore());
                durato = Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedAmp").getScore(p.getName()).getScore();
                break;
            default:
                return;
        }

        if (Math.abs(amplifer) > ampVal)
            addCustomPotionEffect(p, cPotionEffect, amplifer, duration);
        else if (Math.abs(amplifer) == ampVal && durato < duration)
            addCustomPotionEffect(p, cPotionEffect, amplifer, duration);
    }
    static void tryApplyCustomPotionEffect(LivingEntity e, CustomPotionEffect cPotionEffect, int amplifer, int duration) {
        if (duration < 0)
            return;
        amplifer %= 256;
        int ampVal, durato;

        switch (cPotionEffect) {
            case BLEEDING:
                if (!e.getScoreboardTags().contains("bleeding")) {
                    addCustomPotionEffect(e, cPotionEffect, amplifer, duration);
                    return;
                }
                ampVal = Math.abs(Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedTime").getScore(e.getName()).getScore());
                durato = Bukkit.getScoreboardManager().getMainScoreboard().getObjective("bleedAmp").getScore(e.getName()).getScore();
                break;
            default:
                return;
        }
        if (durato < 0)
            return;

        if (Math.abs(amplifer) > ampVal)
            addCustomPotionEffect(e, cPotionEffect, amplifer, duration);
        else if (Math.abs(amplifer) == ampVal && durato < duration)
            addCustomPotionEffect(e, cPotionEffect, amplifer, duration);
    }
}
