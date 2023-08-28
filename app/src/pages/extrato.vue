<script>
import AnalyticsAward from '@/views/dashboard/AnalyticsAward.vue'
import AnalyticsBarCharts from '@/views/dashboard/AnalyticsBarCharts.vue'
import AnalyticsFullBankStatement from '@/views/dashboard/AnalyticsFullBankStatement.vue'
import AnalyticsSalesByCountries from '@/views/dashboard/AnalyticsSalesByCountries.vue'
import AnalyticsTotalEarning from '@/views/dashboard/AnalyticsTotalEarning.vue'
import AnalyticsTotalProfitLineCharts from '@/views/dashboard/AnalyticsTotalProfitLineCharts.vue'
import AnalyticsTransactions from '@/views/dashboard/AnalyticsTransactions.vue'
import AnalyticsUserTable from '@/views/dashboard/AnalyticsUserTable.vue'
import AnalyticsWeeklyOverview from '@/views/dashboard/AnalyticsWeeklyOverview.vue'
import CardStatisticsVertical from '@core/components/cards/CardStatisticsVertical.vue'
import { useCorrentistaStore } from '@/stores/CorrentistaStore'
import { ref } from 'vue'

export default {
  components: {
    AnalyticsAward,
    AnalyticsBarCharts,
    AnalyticsFullBankStatement,
    AnalyticsSalesByCountries,
    AnalyticsTotalEarning,
    AnalyticsTotalProfitLineCharts,
    AnalyticsTransactions,
    AnalyticsUserTable,
    AnalyticsWeeklyOverview,
    CardStatisticsVertical,
  },
  setup() {
    const totalProfit = ref({
      title: 'Total Profit',
      color: 'secondary',
      icon: 'mdi-poll',
      stats: '$25.6k',
      change: 42,
      subtitle: 'Weekly Project',
    })

    const newProject = ref({
      title: 'New Project',
      color: 'primary',
      icon: 'mdi-briefcase-variant-outline',
      stats: '862',
      change: -18,
      subtitle: 'Yearly Project',
    })

    return {
      correntistaId: null,
      totalProfit,
      newProject,
      correntistaStore: useCorrentistaStore(),
    }
  },
  mounted() { },
  methods: {
    
  }
}
</script>


<template>
  <VRow class="match-height">
    <VCol
      cols="12"
      md="4"
    >
      <AnalyticsAward />
    </VCol>

    <VCol
      cols="12"
      md="8"
    >
      <AnalyticsTransactions />
    </VCol>

    <VCol
      cols="12"
      md="12"
    >
      <AnalyticsFullBankStatement />
    </VCol>
  </VRow>
</template>
