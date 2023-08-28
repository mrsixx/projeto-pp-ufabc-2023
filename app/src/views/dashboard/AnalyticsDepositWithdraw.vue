<script setup>
import americanBank from '@images/logos/american-bank.png'
import aws from '@images/logos/aws.png'
import citiBank from '@images/logos/citi-bank.png'
import digitalOcean from '@images/logos/digital-ocean.png'
import github from '@images/logos/github.png'
import google from '@images/logos/google.png'
import gumroad from '@images/logos/gumroad.png'
import mastercardLabel from '@images/logos/mastercard-label.png'
import slack from '@images/logos/slack.png'
import stripe from '@images/logos/stripe.png'
import AnalyticsBankStatementEntry from './AnalyticsBankStatementEntry.vue'


const deposits = [
//   {
//     title: 'Gumroad Account',
//     subtitle: 'Sell UI Kit',
//     amount: 4650,
//     logo: gumroad,
//   },
//   {
//     title: 'Mastercard',
//     subtitle: 'Wallet deposit',
//     amount: 92705,
//     logo: mastercardLabel,
//   },
//   {
//     title: 'Stripe Account',
//     subtitle: 'iOS Application',
//     amount: 957,
//     logo: stripe,
//   },
//   {
//     title: 'American Bank',
//     subtitle: 'American Bank',
//     amount: 6837,
//     logo: americanBank,
//   },
//   {
//     title: 'Bank Account',
//     subtitle: 'Wallet deposit',
//     amount: 8934,
//     logo: citiBank,
//   },
]

const withdraws = [
//   {
//     title: 'Google Adsense',
//     subtitle: 'Paypal deposit',
//     amount: -145.5,
//     logo: google,
//   },
//   {
//     title: 'Github Enterprise',
//     subtitle: 'Security & compliance',
//     amount: -1870,
//     logo: github,
//   },
//   {
//     title: 'Upgrade Slack Plan',
//     subtitle: 'Debit card deposit',
//     amount: -450,
//     logo: slack,
//   },
//   {
//     title: 'Digital Ocean',
//     subtitle: 'Cloud Hosting',
//     amount: -540,
//     logo: digitalOcean,
//   },
//   {
//     title: 'AWS Account',
//     subtitle: 'Choosing a Cloud Platform',
//     amount: -21,
//     logo: aws,
//   },
]
</script>

<template>
  <VCard>
    <VRow no-gutters>
      <VCol
        cols="12"
        md="6"
      >
        <VCardItem>
          <VCardTitle>Entradas</VCardTitle>

          <template #append>
            <a
              href="javascript:void(0)"
              class="text-sm font-weight-medium"
            >View All</a>
          </template>
        </VCardItem>

        <VCardText>
          <VList class="card-list">
            <AnalyticsBankStatementEntry
              v-for="(deposit, i) in deposits"
              :key="i"
              :entry="deposit"/>
          </VList>
        </VCardText>
      </VCol>

      <VDivider
        :vertical="$vuetify.display.mdAndUp"
        :inset="$vuetify.display.mdAndUp"
      />

      <VCol
        cols="12"
        md="6"
      >
        <VCardItem>
          <VCardTitle>Saídas</VCardTitle>

          <template #append>
            <router-link
              to="/extrato"
              class="text-sm font-weight-medium"
            >View All</router-link>
          </template>
        </VCardItem>

        <VCardText>
          <VList class="card-list">
            <AnalyticsBankStatementEntry
              v-for="(withdraw, i) in withdraws"
              :key="i"
              :entry="withdraw"/>
          </VList>
        </VCardText>
      </VCol>
    </VRow>
  </VCard>
</template>

<style lang="scss" scoped>
.card-list {
  --v-card-list-gap: 1.5rem;
}
</style>
